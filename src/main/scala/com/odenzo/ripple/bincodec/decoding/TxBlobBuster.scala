package com.odenzo.ripple.bincodec.decoding

import scala.annotation.tailrec

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.UByte

import com.odenzo.ripple.bincodec.codecs.{MoneyCodecs, PathCodecs, STArrayCodec, STObjectCodec, VLEncoding}
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.reference.{Definitions, FieldInfo}
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{Decoded, DecodedField, DecodedNestedField, RawValue}

/** Development helper, not completed */
trait TxBlobBuster extends StrictLogging with JsonUtils with ByteUtils with CodecUtils {

  def bust(txBlob: String): Either[RippleCodecError, List[Decoded]] = {
    val ubytes: Either[RippleCodecError, List[UByte]] = hex2ubytes(txBlob)
    ubytes.flatMap(decode(_, false))
  }

  /** Go through and break apart in encoded fields. Not a blob or STObject for top (double check) */
  def decode(blob: List[UByte], nested: Boolean): Either[RippleCodecError, List[Decoded]] = {
    // Take Field Marker

    def loop(blob: List[UByte], acc: List[Decoded]): Either[RippleCodecError, List[Decoded]] = {
      if (blob.isEmpty) acc.reverse.asRight
      else {
        decodeNextField(blob) match {
          case Left(err)                  ⇒ err.asLeft
          case Right((fd: Decoded, togo)) ⇒ loop(togo, fd :: acc)
        }
      }
    }

    loop(blob, List.empty[DecodedField])
  }

  def decodeNextField(blob: List[UByte]): Either[OErrorRipple, (Decoded, List[UByte])] = {
    decodeNextFieldId(blob).flatMap {
      case (info, ub) ⇒
        decodeField(info, ub)
    }
  }

  /** First cut just do to hex */
  def decodeField(info: FieldInfo, blob: List[UByte]): Either[OErrorRipple, (Decoded, List[UByte])] = {
    if (info.isVLEncoded) { // TODO: Special case for AccountID, not nested then vl encoded else not
      // Decode the VL then just leave it as hex for now.
      //   case "Blob"   ⇒ encodeBlob(fieldValue) is VL Encoded
      val vled = VLEncoding.decodeVL(blob).flatMap { case (len, data) ⇒ decodeToUBytes(len, data, info) }
      vled
    } else {
      info.datatype.name match {

        case "AccountID" ⇒ decodeToUBytes(20, blob, info)
        case "UInt8"     ⇒ decodeToUBytes(1, blob, info)
        case "UInt16"    ⇒ decodeToUBytes(2, blob, info)
        case "UInt32"    ⇒ decodeToUBytes(4, blob, info)
        case "UInt64"    ⇒ decodeToUBytes(8, blob, info)
        case "Hash160"   ⇒ decodeToUBytes(20, blob, info)
        case "Hash256"   ⇒ decodeToUBytes(32, blob, info)
        // Below here are container fields, in the case of amount sometimes
        case "Amount"   ⇒ MoneyCodecs.decodeAmount(blob, info)
        case "STArray"  ⇒ STArrayCodec.decodeSTArray(blob, info)
        case "STObject" ⇒ STObjectCodec.decodeSTObject(blob, info)
        case "PathSet"  ⇒ PathCodecs.decodePathSet(blob, info)
//        case "Vector256" ⇒ ContainerFields.encodeVector256(fieldData)

        case other ⇒
          logger.error(s"Not Decoding Field Type $other")
          RippleCodecError(s"Not Decoding Field Type $other").asLeft

      }
    }

  }





  /**
    *   Check the next 1 to 3 bytes for a FieldID and lookup the field info
    *   This is round-tripping instead of taking the shortcut just to see
    *   how many bytes to encoded marker takes up.s
    * @param blob Array of unsigned bytes
    * @return FieldInfomation and the remaining UBytes that were not fieldid
    */
  def decodeNextFieldId(blob: List[UByte]): Either[OErrorRipple, (FieldInfo, List[UByte])] = {
    logger.info(s"Decoding Field ID from ${blob.take(6)}...")
    val ZERO             = UByte.MinValue
    val TOP_FOUR_MASK    = UByte(0xF0)
    val BOTTOM_FOUR_MASK = UByte(0x0F)

    val top    = blob.head & TOP_FOUR_MASK
    val bottom = blob.head & BOTTOM_FOUR_MASK
    logger.info(s"Top $top Bottom $bottom")
    
    val (fCode, tCode, blobRemaining) = (top === ZERO, bottom === ZERO) match {
      case (false, false) ⇒ // One Byte
        val typecode = top >> 4
        val fieldcode  = bottom 
        (fieldcode, typecode, blob.drop(1))

      case (false, true) ⇒ // 2 byte typecode top
        val typecode  = blob.head >> 4
        val fieldcode = blob(1)
        (fieldcode, typecode, blob.drop(2))

      case (true, false) ⇒
        val fieldcode = blob.head
        val typecode  = blob(1)
        (fieldcode, typecode, blob.drop(2))
      case (true, true) ⇒ // 3 byte case
        val typecode  = blob(1)
        val fieldcode = blob(2)
        (fieldcode, typecode, blob.drop(3))
    }
          // Actually, we coudl just find by fieldCode!
    logger.info(s"Potential Fields\n:" + Definitions.fieldData.getFieldsByNth(fCode.toLong).mkString("\n"))
    val fieldMarker: List[UByte] = FieldInfo.encodeFieldID(fCode.toInt, tCode.toInt)
    Definitions.fieldData.findByFieldMarker(fieldMarker) match {
      case None     ⇒ RippleCodecError(s"No FieldData found for Marker $fieldMarker").asLeft
      case Some(fd) ⇒ (fd._2, blobRemaining).asRight
    }
  }

}

object TxBlobBuster extends TxBlobBuster