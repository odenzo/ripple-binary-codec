package com.odenzo.ripple.bincodec.encoding

import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe._
import io.circe.syntax._
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.codecs.{AccountIdCodecs, ContainerFields, MoneyCodecs, PathCodecs, VLEncoding}
import com.odenzo.ripple.bincodec.reference.{DefinitionData, FieldData, RippleDataType}
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{Encoded, EncodedDataType, EncodedField, EncodedNestedVals, RawValue}

/**
  * I am not building these based on pure JSON rather
  * than the WSModel objects
  */
object TypeSerializers extends StrictLogging with JsonUtils with CodecUtils {

  def encodeTopLevel(json: Json, isSigning: Boolean): Either[RippleCodecError, EncodedNestedVals] = {

    ContainerFields.encodeSTObject(json, isNested = false, isSigning = isSigning)
  }

  /**
    * Encodes a field and value. It seems we need to know if the field is in a nested JSObject
    * in order to not VL Encode Account="r...", e.g. in txJson
    * A field here by definition has a fieldName and associated meta data.
    *
    * Note that encoding a field may produce nested structure, working on tht.
    *
    * <p>
    *     Meh, need some conventions regarding VLEncoding structure and also
    *     when an incoder should place the FieldType bytes in front or not.
    *     There is no real need to do when being called from here.
    *     When passed in a FieldData it SHOULD NOT, as FieldEncoded will handle it.
    *     Values in FieldEncoded SHOULD NOT have field keys in them.
    *     But when we pass in Json and get RawEncoded it usually has it now.
    *     Problem is things like account that differs on nesting.
    *     So, lets simplify a bit more and see whats best in regards to VLEncoding and
    *     FieldType tag.
    *     Note that array and object end stuff always embedded in raw vlaue.
    * </p>
    * @param fieldData
    *
    * @return
    */
  def encodeFieldAndValue(fieldData: FieldData,
                          isNestedObject: Boolean,
                          signingModeOn: Boolean): Either[RippleCodecError, EncodedField] = {
    val fieldName: String = fieldData.fieldName
    val fieldValue: Json  = fieldData.v

    logger.debug(s"Encoding FieldValue: $fieldData")

    val valueBytes: Either[RippleCodecError, Encoded] = fieldData.fi.fieldTypeName match {
      case "UInt16" if fieldName === "LedgerEntryType" ⇒ encodeLedgerEntryType(fieldValue)
      case "UInt16" if fieldName === "TransactionType" ⇒ encodeTransactionType(fieldValue)
      case "AccountID" if isNestedObject               ⇒ AccountIdCodecs.encodeAccountNoVL(fieldValue)
      case "AccountID" if !isNestedObject              ⇒ AccountIdCodecs.encodeAccount(fieldValue)

      case "UInt8"     ⇒ encodeUIntN(fieldValue, "UInt8")
      case "UInt16"    ⇒ encodeUIntN(fieldValue, "UInt16")
      case "UInt32"    ⇒ encodeUIntN(fieldValue, "UInt32")
      case "UInt64"    ⇒ encodeUInt64(fieldValue)
      case "Hash160"   ⇒ encodeHash160(fieldValue)
      case "Hash256"   ⇒ encodeHash256(fieldValue)
      case "Blob"      ⇒ encodeBlob(fieldValue)
      case "Amount"    ⇒ MoneyCodecs.encodeAmount(fieldValue)
      case "PathSet"   ⇒ PathCodecs.encodePathSet(fieldData)
      case "Vector256" ⇒ ContainerFields.encodeVector256(fieldData)
      case "STArray"   ⇒ ContainerFields.encodeSTArray(fieldData, signingModeOn)
      case "STObject"  ⇒ ContainerFields.encodeSTObject(fieldValue, isNestedObject, signingModeOn)

      case other ⇒ RippleCodecError(s"Not handling Field Type $other").asLeft

    }

    // Lets drag some baggage along!
    val full: Either[RippleCodecError, EncodedField] = valueBytes.map(EncodedField(_, fieldData))
    full

  }

  /** Encodes the hex including the Variable Length info */
  def encodeBlob(json: Json): Either[RippleCodecError, RawValue] = {
    for {
      str    <- JsonUtils.decode(json, Decoder[String])
      ubytes ← ByteUtils.hex2ubytes(str)
      rev    ← VLEncoding.prependVL(ubytes)
    } yield rev

  }

  def encodeHash(json: Json, byteLen: Int): Either[RippleCodecError, RawValue] = {
    // This looks like Hex Already... in fact just round tripping
    val str = JsonUtils.decode(json, Decoder[String])
    val ans: Either[RippleCodecError, List[UByte]] = str.flatMap { v ⇒
      ByteUtils.hex2ubytes(v)
    }
    val checked = ans.flatMap(ByteUtils.ensureMaxLength(_, byteLen))
    checked.map(ByteUtils.zeroPadBytes(_, byteLen))
    ans.fmap(RawValue)
  }

  def encodeHash160(json: Json): Either[RippleCodecError, EncodedDataType] = {
    val rtype: Either[OErrorRipple, RippleDataType]            = dd.getTypeObj("Hash160")
    val encoded: Either[RippleCodecError, RawValue] = encodeHash(json, 20)

    (encoded, rtype).mapN(EncodedDataType)
  }

  def encodeHash256(json: Json): Either[RippleCodecError, EncodedDataType] = {
    val rtype: Either[OErrorRipple, RippleDataType]            = dd.getTypeObj("Hash256")
    val encoded: Either[RippleCodecError, RawValue] = encodeHash(json, 32)

    (encoded, rtype).mapN(EncodedDataType(_, _))
  }

  def encodeTransactionType(json: Json): Either[RippleCodecError, RawValue] = {
    val res: Either[RippleCodecError, RawValue] =
      JsonUtils.decode(json, Decoder[String]).flatMap(v ⇒ txnType2Bin(v))
    res
  }

  def encodeLedgerEntryType(json: Json): Either[RippleCodecError, RawValue] = {
    val res: Either[RippleCodecError, RawValue] =
      JsonUtils.decode(json, Decoder[String]).flatMap(v ⇒ ledgerType2Bin(v))
    res
  }

  def encodeTxnResultType(json: Json): Either[RippleCodecError, RawValue] = {
    json2string(json).flatMap(txnResultType2Bin)
  }

  /** Adapted from the Javascript
    * Redo with one UInt Decoder and a function for each type.
    * */
  def encodeUIntN(v: Json, dataType: String): Either[RippleCodecError, RawValue] = {
    BinCodecExeption.wrap(s"Binary Encoding JSON # $v") {

      val number: Option[ULong] = for {
        numeric <- v.asNumber
        ulong   <- numeric.toLong.map(ULong(_))
      } yield ulong

      val unsigned = Either.fromOption(number, RippleCodecError(s"JSON ${v.spaces2} was not a Unisgned Long Number"))

      val ans: Either[OErrorRipple, RawValue] = unsigned.flatMap(v ⇒ encodeULong(v, dataType))
      ans
    }
  }

  def encodeUInt64(json: Json): Either[RippleCodecError, RawValue] = {
    parseUInt64(json).flatMap(encodeULong(_, "UInt64"))
  }

  /** TODO: Potentially overflow, check thus esp UInt64 */
  def encodeULong(value: ULong, dataType: String): Either[Nothing, RawValue] = {
    val fieldLength = dataType match {
      case "UInt8"  ⇒ 1
      case "UInt16" ⇒ 2
      case "UInt32" ⇒ 4
      case "UInt64" ⇒ 8
      case other    ⇒ throw new IllegalArgumentException(s"$other was not a valid unsigned int type")
    }

    val bytes: Either[Nothing, List[UByte]] = (0 until fieldLength)
      .map { i: Int ⇒
        val calcUnsigned: ULong = value >>> (i * 8)
        UByte(calcUnsigned.toByte)
      }
      .toList
      .reverse
      .asRight
    bytes.foreach(bl ⇒ assert(bl.length == fieldLength))
    bytes.map(RawValue)
  }

  /** Given a transaction type name, an enumeration basically, return its value. -1 invalid until 101 Error if not
    * found. I think these are always UInt16 incoded.
    * */
  def txnType2Bin(txnName: String): Either[RippleCodecError, RawValue] = {
    dd.getTransactionType(txnName).flatMap(l ⇒ encodeUIntN(Json.fromLong(l), "UInt16"))

  }

  def ledgerType2Bin(entryType: String): Either[RippleCodecError, RawValue] = {
    dd.getLedgerEntryType(entryType).flatMap(l ⇒ encodeUIntN(Json.fromLong(l), "UInt16"))
  }

  // This should be pre-baked of course.
  def txnResultType2Bin(entryType: String): Either[RippleCodecError, RawValue] = {
    dd.getTxnResultType(entryType).flatMap(l ⇒ encodeUIntN(Json.fromLong(l), "UInt16"))
  }
}
