package com.odenzo.ripple.bincodec.deserialize

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe._
import io.circe.syntax._
import spire.math.UByte

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.bincodec.reference.{Definitions, FieldInfo}
import com.odenzo.ripple.bincodec.serializing.BinarySerializer._
import com.odenzo.ripple.bincodec.serializing.{BinarySerializer, VLEncoding}
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}

/** Development helper, not completed */
trait TxBlobBuster extends StrictLogging with JsonUtils with ByteUtils {

  /** Go through and break apart in encoded fields. Not a blob or STObject for top (double check) */
  def decode(txBlob: List[UByte]) = {
    // Take Field Marker

  }

  def decodeNextField(blob: List[UByte]): Either[OErrorRipple, (FieldDecoded, List[UByte])] = {
    val fieldId: Either[OErrorRipple, (FieldInfo, List[UByte])] = decodeNextFieldId(blob)

    fieldId.flatMap{ case (info,ub) ⇒
      decodeField(info,ub)
    }

  }

  /** First cut just do to hex */
  def decodeField(info: FieldInfo, blob: List[UByte]): Either[OErrorRipple, (FieldDecoded, List[UByte])] = {
    if (info.isVLEncoded) { // TODO: Special case for AccountID, not nested then vl encoded else not
      // Decode the VL then just leave it as hex for now.
      //   case "Blob"   ⇒ encodeBlob(fieldValue) is VL Encoded
      val vled = VLEncoding.decodeVL(blob).flatMap { case (len, data) ⇒ decodeToUBytes(len, data, info) }
            vled
    } else {
      info.fieldType.name match {

        case "AccountID" ⇒ decodeToUBytes(20, blob, info)

        case "UInt8"  ⇒ decodeToUBytes(1, blob, info)
        case "UInt16" ⇒ decodeToUBytes(2, blob, info)
        case "UInt32" ⇒ decodeToUBytes(4, blob, info)
        case "UInt64" ⇒ decodeToUBytes(8, blob, info)

        case "Hash160" ⇒ decodeToUBytes(20, blob, info)
        case "Hash256" ⇒ decodeToUBytes(32, blob, info)

        // Below here are container fields, in the case of amount sometimes
        case "Amount"   ⇒ decodeAmount(blob, info)
        case "STArray"  ⇒ decodeSTArray(blob, info)
        case "STObject" ⇒ decodeSTObject(blob, info) // May need to know if nested
        case "PathSet"  ⇒ decodePathSet(blob, info)
//        case "Vector256" ⇒ ContainerFields.encodeVector256(fieldData)

        case other ⇒ RippleCodecError(s"Not Decoding Field Type $other").asLeft

      }

      //    case "UInt16" if fieldName === "LedgerEntryType" ⇒ encodeLedgerEntryType(fieldValue)
      //        case "UInt16" if fieldName === "TransactionType" ⇒ encodeTransactionType(fieldValue)
      //
    }

  }

  def decodeSTArray(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (FieldDecodedNested, List[UByte])] = {

    val endOfFieldsMarker = UByte(0xF1)
    // info is for the top level array
    def subfields(ub: List[UByte], acc: List[FieldDecoded]): Either[OErrorRipple, (List[FieldDecoded], List[UByte])] = {
      if (ub.head === endOfFieldsMarker) {
        (acc.reverse, ub.drop(1)).asRight
      } else {
        val next: Either[OErrorRipple, (FieldDecoded, List[UByte])] = decodeNextField(v)
        next.flatMap {
          case (field, tail) ⇒
            subfields(tail, field :: acc)
        }
      }
    }
    subfields(v, List.empty[FieldDecoded]).map {
      case (fields, rest) ⇒
        (FieldDecodedNested(info, fields), rest)
    }
  }

  def decodeSTObject(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (FieldDecodedNested, List[UByte])] = {
    val endOfSTObjectMarker = UByte(0xE1)
    // info is for the top level array
    def subfields(ub: List[UByte], acc: List[FieldDecoded]): Either[OErrorRipple, (List[FieldDecoded], List[UByte])] = {
      if (ub.head === endOfSTObjectMarker) {
        (acc.reverse, ub.drop(1)).asRight
      } else {
        val next: Either[OErrorRipple, (FieldDecoded, List[UByte])] = decodeNextField(v)
        next.flatMap {
          case (field, tail) ⇒
            subfields(tail, field :: acc)
        }
      }
    }

    subfields(v, List.empty[FieldDecoded]).map {
      case (fields, rest) ⇒
        (FieldDecodedNested(info, fields), rest)
    }
  }

  def decodePathSet(v: List[UByte], info: FieldInfo): Either[Nothing, (FieldDecodedNested, List[UByte])] = {
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
    (FieldDecodedNested(info, flat), tail).asRight
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

  def decodeAmount(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (FieldDecodedUBytes, List[UByte])] = {
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
                     info: FieldInfo): Either[OErrorRipple, (FieldDecodedUBytes, List[UByte])] = {
    if (numBytes > v.length) RippleCodecError(s"$numBytes exceeded length ${v.length} decoding").asLeft
    else {
      val (taken: List[UByte], remaining) = v.splitAt(numBytes)
      (FieldDecodedUBytes(info, taken), remaining).asRight
    }
  }

  /**
    *   Check the next 1 to 3 bytes for a FieldID and lookup the field info
    * @param blob Array of unsigned bytes
    * @return FieldInfomation and the remaining UBytes that were not fieldid
    */
  def decodeNextFieldId(blob: List[UByte]): Either[OErrorRipple, (FieldInfo, List[UByte])] = {

    val ZERO             = UByte.MinValue
    val TOP_FOUR_MASK    = UByte(0xF0)
    val BOTTOM_FOUR_MASK = UByte(0x0F)

    val top    = blob.head & BOTTOM_FOUR_MASK
    val bottom = blob.head & TOP_FOUR_MASK

    // One Byte Case
    val (fCode, tCode, blobRemaining) = (top === ZERO, bottom === ZERO) match {
      case (false, false) ⇒ // One Byte
        val fieldcode = blob.head & BOTTOM_FOUR_MASK
        val typecode  = (blob.head & TOP_FOUR_MASK) >> 4
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
    val fieldMarker = ByteUtils.ubyte2hex(fCode)
    val fieldData   = Definitions.fieldData.findByFieldMarker(fieldMarker)

    fieldData match {
      case None     ⇒ RippleCodecError(s"No FieldData found for Marker $fieldMarker").asLeft
      case Some(fd) ⇒ (fd._2, blobRemaining).asRight
    }
  }

  def decomposeTxBlob(txJson: JsonObject, txBlob: String) = {

    logger.info(s"txJson IN: ${txJson.asJson.spaces4}")
    // Note the hash is not included in the TxBlob, but *after* signing the TxnSignature is.

    // Testing utility really. Serializing the transaction, and then start removing
    // all serialized fields to see what it left.
    // It is my assumption to test that the payload signed is TxBlob - TxnSignature
    // Also to check if I am missing some mandatory fields.
    var remainingBlob = txBlob

    // I think hash and TxnSignature are not in definitions...
    val serialized: Either[RippleCodecError, BinarySerializer.NestedEncodedValues] =
      RippleCodecAPI.binarySerialize(txJson)

    serialized.foreach { fields =>
      fields.enclosed.foreach {
        case x: FieldEncoded => logger.info(s" Field: ${x.data.fi.fieldID.toHex} " + x.data.key)
        case other           => logger.info(s"OTHER: ${other.toHex}")
      }

      fields.encoded.foreach { rev =>
        val enc = rev.toHex
        remainingBlob = excise(remainingBlob, enc)
      }
    }
    logger.info(s"Remaining Blob: $remainingBlob")

    // Lets try hashing the TxBlob as is (which Txn Prefix)
    // Go thru the encoded and remove the TxnSignature (74) from txblob
    val noTxSig: Either[RippleCodecError, List[BinarySerializer.Encoded]] = for {
      encoded ← serialized
      filtered = encoded.enclosed.flatMap {
        case field: FieldEncoded if field.data.key == "TxnSignature" ⇒ None
        case other                                                   ⇒ Some(other)
      }
    } yield filtered

    val rawBytes   = noTxSig.map(list ⇒ list.foldLeft(List.empty[UByte])(_ ::: _.rawBytes))
    val updateBlob = rawBytes.map(ubytes2hex)
    updateBlob.foreach { ub ⇒
      logger.info(s"Blob with no TxnSignature:\n $ub \n $txBlob")
      logger.info(s"Len ${ub.length} vs ${txBlob.length}")
    }

  }

  def excise(data: String, toCut: String) = {
    logger.debug(s"Cutting $toCut from $data")
    val res = data.replaceFirst(toCut, "")
    if (res == data) throw new IllegalStateException(s"$toCut not found in $data")
    res
  }
}
