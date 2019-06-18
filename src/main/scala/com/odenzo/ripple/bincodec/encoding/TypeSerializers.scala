package com.odenzo.ripple.bincodec.encoding

import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe._
import io.circe.syntax._
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.codecs.{
  AccountIdCodecs,
  ContainerFields,
  HashHexCodecs,
  MoneyCodecs,
  PathCodecs,
  UIntCodecs,
  VLEncoding
}
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

      case "UInt8"     ⇒ UIntCodecs.encodeUIntN(fieldValue, "UInt8")
      case "UInt16"    ⇒ UIntCodecs.encodeUIntN(fieldValue, "UInt16")
      case "UInt32"    ⇒ UIntCodecs.encodeUIntN(fieldValue, "UInt32")
      case "UInt64"    ⇒ UIntCodecs.encodeUInt64(fieldValue)
      case "Hash160"   ⇒ HashHexCodecs.encodeHash160(fieldValue)
      case "Hash256"   ⇒ HashHexCodecs.encodeHash256(fieldValue)
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

  /** Given a transaction type name, an enumeration basically, return its value. -1 invalid until 101 Error if not
    * found. I think these are always UInt16 incoded.
    * */
  def txnType2Bin(txnName: String): Either[RippleCodecError, RawValue] = {
    dd.getTransactionType(txnName).flatMap(l ⇒ UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16"))

  }

  def ledgerType2Bin(entryType: String): Either[RippleCodecError, RawValue] = {
    dd.getLedgerEntryType(entryType).flatMap(l ⇒ UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16"))
  }

  // This should be pre-baked of course.
  def txnResultType2Bin(entryType: String): Either[RippleCodecError, RawValue] = {
    dd.getTxnResultType(entryType).flatMap(l ⇒ UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16"))
  }
}
