package com.odenzo.ripple.bincodec.decoding

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.UByte

import com.odenzo.ripple.bincodec.codecs.VLEncoding
import com.odenzo.ripple.bincodec.reference.{Definitions, FieldInfo}
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{Decoded, DecodedField, DecodedNestedField, DecodedUBytes}

/** Development helper, not completed */
trait TxBlobBuster extends StrictLogging with JsonUtils with ByteUtils {

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
        case "Amount"   ⇒ decodeAmount(blob, info)
        case "STArray"  ⇒ decodeSTArray(blob, info)
        case "STObject" ⇒ decodeSTObject(blob, info) // May need to know if nested
        case "PathSet"  ⇒ decodePathSet(blob, info)
//        case "Vector256" ⇒ ContainerFields.encodeVector256(fieldData)

        case other ⇒
          logger.error(s"Not Decoding Field Type $other")
          RippleCodecError(s"Not Decoding Field Type $other").asLeft

      }
    }

  }

  def decodeSTArray(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (DecodedNestedField, List[UByte])] = {
    val endOfFieldsMarker = UByte(0xF1)
    // info is for the top level array
    def subfields(ub: List[UByte], acc: List[Decoded]): Either[OErrorRipple, (List[Decoded], List[UByte])] = {
      if (ub.head === endOfFieldsMarker) {
        (acc.reverse, ub.drop(1)).asRight
      } else {
        val next: Either[OErrorRipple, (Decoded, List[UByte])] = decodeNextField(ub)
        logger.debug(s"Next: $next")
        next match {
          case Left(err)            ⇒ err.asLeft
          case Right((field, tail)) ⇒ subfields(tail, field :: acc)
        }
      }
    }
    subfields(v, List.empty[DecodedField]).map {
      case (fields, rest) ⇒
        (DecodedNestedField(info, fields), rest)
    }
  }

  def decodeSTObject(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (DecodedNestedField, List[UByte])] = {
    val endOfSTObjectMarker = UByte(0xE1)
    // info is for the top level array
    @scala.annotation.tailrec
    def subfields(ub: List[UByte], acc: List[Decoded]): Either[OErrorRipple, (List[Decoded], List[UByte])] = {
      if (ub.head === endOfSTObjectMarker) {
        (acc.reverse, ub.drop(1)).asRight
      } else {
        val next: Either[OErrorRipple, (Decoded, List[UByte])] = decodeNextField(ub)
        logger.debug(s"STObject Next: $next")
        next match {
          case Left(err)            ⇒ err.asLeft
          case Right((field, tail)) ⇒ subfields(tail, field :: acc)
        }
      }
    }

    subfields(v, List.empty[Decoded]).fmap {
      case (fields, rest) ⇒
        (DecodedNestedField(info, fields), rest)
    }
  }

  def decodePathSet(v: List[UByte], info: FieldInfo): Either[Nothing, (DecodedNestedField, List[UByte])] = {
    // Array of Arrays

    def loop(v: List[UByte], acc: List[List[DecodedUBytes]]): (List[List[DecodedUBytes]], List[UByte]) = {
      val (path, tail) = decodePath(v)
      if (path.head.value.head === UByte.MinValue) {
        (path.reverse :: acc, tail)
      } else {
        loop(v, path.reverse :: acc)
      }
    }
    val (lld: List[List[DecodedUBytes]], tail) = loop(v, List.empty[List[DecodedUBytes]])
    val flat: List[DecodedUBytes]              = lld.flatten // Not ideal for no Nested w/o Field yet
    (com.odenzo.ripple.bincodec.DecodedNestedField(info, flat), tail).asRight
  }

  /**
    *
    * @param path
    * @return A list of decoded ubytes for the path, in reverse order.
    */
  def decodePath(path: List[UByte]): (List[DecodedUBytes], List[UByte]) = {
    // Composed of N Path Steps , first always has implied account
    val endPathWithMorePaths = UByte(0xFF)
    val endPathAndPathSet    = UByte(0x00)

    @scala.annotation.tailrec
    def loop(v: List[UByte], acc: List[DecodedUBytes]): (List[DecodedUBytes], List[UByte]) = {
      if (v.head === endPathWithMorePaths || v.head == endPathAndPathSet) {
        val marker = DecodedUBytes(List(v.head))
        (marker :: acc, v.drop(1))
      } else {
        val (decoded, tail) = decodePathStep(v)
        loop(tail, decoded :: acc)
      }
    }

    loop(path, List.empty[DecodedUBytes])
  }

  def decodePathStep(v: List[UByte]): (DecodedUBytes, List[UByte]) = {
    val kZERO         = UByte(0)
    val kAddressStep  = UByte(1)
    val kCurrencyStep = UByte(16)
    val kIssuerStep   = UByte(32)

    // Since just going to Hex and address currency and issuer all 20 we cheat
    val stepType = v.head

    val numBits = List(
      (stepType & kAddressStep) > kZERO,
      (stepType & kCurrencyStep) > kZERO,
      (stepType & kIssuerStep) > kZERO
    ).filterNot(_ === true).length

    val len             = 1 + (numBits * 20) // Including the marker
    val (decoded, tail) = v.splitAt(len)
    (DecodedUBytes(decoded), tail)
  }

  def decodeAmount(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (DecodedField, List[UByte])] = {
    val TOP_BIT_MASK: UByte = UByte(128)
    if ((v.head & TOP_BIT_MASK) == UByte(0)) { //XRP
      decodeToUBytes(8, v, info)
    } else { // Fiat
      decodeToUBytes(48, v, info)
    }
  }

  /** Decodes field bytes to hex with no padding */
  def decodeToUBytes(numBytes: Int,
                     v: List[UByte],
                     info: FieldInfo): Either[OErrorRipple, (DecodedField, List[UByte])] = {
    if (numBytes > v.length) RippleCodecError(s"$numBytes exceeded length ${v.length} decoding").asLeft
    else {
      val (taken: List[UByte], remaining) = v.splitAt(numBytes)
      (DecodedField(info, taken), remaining).asRight
    }
  }

  /**
    *   Check the next 1 to 3 bytes for a FieldID and lookup the field info
    * @param blob Array of unsigned bytes
    * @return FieldInfomation and the remaining UBytes that were not fieldid
    */
  def decodeNextFieldId(blob: List[UByte]): Either[OErrorRipple, (FieldInfo, List[UByte])] = {
    logger.info(s"Decoding Field ID from ${blob.take(6)}...")
    val ZERO             = UByte.MinValue
    val TOP_FOUR_MASK    = UByte(0xF0)
    val BOTTOM_FOUR_MASK = UByte(0x0F)

    val top    = blob.head & BOTTOM_FOUR_MASK
    val bottom = blob.head & TOP_FOUR_MASK

    val (fCode, tCode, blobRemaining) = (top === ZERO, bottom === ZERO) match {
      case (false, false) ⇒ // One Byte
        val fieldcode = top
        val typecode  = bottom >> 4
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

    val fieldMarker = FieldInfo.encodeFieldID(fCode.toInt, tCode.toInt)
    Definitions.fieldData.findByFieldMarker(fieldMarker) match {
      case None     ⇒ RippleCodecError(s"No FieldData found for Marker $fieldMarker").asLeft
      case Some(fd) ⇒ (fd._2, blobRemaining).asRight
    }
  }

}

object TxBlobBuster extends TxBlobBuster
