package com.odenzo.ripple.bincodec.encoding

import cats.data._
import cats.implicits._
import io.circe.Json

import com.odenzo.ripple.bincodec.codecs.{AccountIdCodecs, ContainerFields, HashHexCodecs, MoneyCodecs, PathCodecs, UIntCodecs, VLEncoding}
import com.odenzo.ripple.bincodec.reference.FieldData
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{Encoded, EncodedField, EncodedNestedVals, EncodedVL, RawValue}

/**
  * I am not building these based on pure JSON rather
  * than the WSModel objects
  */
object TypeSerializers  extends JsonUtils with CodecUtils {

  def encodeTopLevel(json: Json, isSigning: Boolean): Either[RippleCodecError, EncodedNestedVals] = {

    ContainerFields.encodeSTObject(json, isNested = false, isSigning = isSigning)
  }


//
//  def deepFilterJsonObject(o:JsonObject, isSigning:Boolean):JsonObject = {
//    o.toList.flatMap{ case (fieldName, fieldVal) ⇒
//      dd.optFieldData(fieldName, fieldVal)
//    }
//  }
//
//  /**
//    *
//    * Canonically sorts a json object and removes non-serializable or non-signing fields
//    * TODO: Should just do this as a deep traverse once at the begining and avoid passing isSigning around.
//    *
//    * @param o
//    * @param isSigning Remove all non-signing fields if true, else serialized
//    *
//    * @return
//    */
//  def prepareJsonObject(o: JsonObject, isSigning: Boolean): Either[RippleCodecError, List[FieldData]] = {
//    logger.trace(s"prepareJsonObect ")
//    val bound: List[FieldData] = o.toList.flatMap{   case (fieldName, fieldVal) ⇒
//      dd.optFieldData(fieldName, fieldVal)
//    }
//    val filtered = if (isSigning) {
//      bound.filter(_.fi.isSigningField)
//    } else {
//      bound.filter(_.fi.isSerialized)
//    }
//
//    filtered.sortBy(_.fi.sortKey).asRight
//  }

  /**
    * Encodes a field and value. It seems we need to know if the field is in a nested JSObject
    * in order to not VL Encode Account="r...", e.g. in txJson
    * A field here by definition has a fieldName and associated meta data.
    *
    * Note that encoding a field may produce nested structure
    *
    * @param fieldData
    *
    * @return
    */
  def encodeFieldAndValue(fieldData: FieldData,
                          isNestedObject: Boolean,
                          signingModeOn: Boolean): Either[RippleCodecError, EncodedField] = {
    val fieldName: String = fieldData.fieldName
    val fieldValue: Json  = fieldData.v

    scribe.debug(s"Encoding FieldValue: $fieldData")

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
  def encodeBlob(json: Json): Either[RippleCodecError, EncodedVL] = {
    for {
      str    <- json2string(json)
      ubytes ← ByteUtils.hex2ubytes(str)
      rev    ← VLEncoding.prependVL(ubytes)
    } yield rev

  }

  def encodeTransactionType(json: Json): Either[RippleCodecError, RawValue] = {
    json2string(json)
      .flatMap(dd.getTransactionType)
      .flatMap(l ⇒ UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16")) // PreBake
  }

  def encodeLedgerEntryType(json: Json): Either[RippleCodecError, RawValue] = {
    json2string(json)
      .flatMap(dd.getLedgerEntryType)
      .flatMap(l ⇒ UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16")) // PreBake
  }

  def encodeTxnResultType(json: Json): Either[RippleCodecError, RawValue] = {
    json2string(json)
      .flatMap(dd.getTxnResultType)
      .flatMap(l ⇒ UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16")) // PreBake
  }

}
