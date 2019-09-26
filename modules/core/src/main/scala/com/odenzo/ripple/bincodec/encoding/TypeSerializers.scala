package com.odenzo.ripple.bincodec.encoding

import cats.data._
import cats.implicits._
import io.circe.Json

import com.odenzo.ripple.bincodec.codecs._
import com.odenzo.ripple.bincodec.reference.FieldData
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.{EncodedSTObject, EncodedField, BinCodecLibError, Encoded}

/**
  *  Goes throught and deep serializes a JsonObject
  */
object TypeSerializers extends JsonUtils with CodecUtils {

  /** The very top level object, which doesn't get an end of object marker */
  def encodeTopLevel(json: Json, isSigning: Boolean): Either[BinCodecLibError, EncodedSTObject] = {
    ContainerFields
      .encodeSTObject(json, isSigning = isSigning)
      .map(top => top.copy(isTopLevel = true))
  }

  /**
    *All subsequent JsonObject fields come through here to get decoded.
    * Encodes a field and value. It seems we need to know if the field is in a nested JSObject
    * in order to not VL Encode Account="r...", e.g. in txJson
    * A field here by definition has a fieldName and associated meta data.
    *
    * Note that encoding a field may produce nested structure
    *
    * @param fieldData The actual Jaon for the field with the FieldMetaData
    * @param signingModeOn True if we are encoding for signing vs full serialization
    * @return
    */
  def encodeFieldAndValue(
      fieldData: FieldData,
      signingModeOn: Boolean
  ): Either[BinCodecLibError, EncodedField] = {

    val fieldName: String = fieldData.fieldName
    val fieldValue: Json  = fieldData.json

    scribe.debug(s"Encoding FieldValue: $fieldData")

    // Could bind the encoder to fieldData but I think this way is clearer

    val valueBytes: Either[BinCodecLibError, Encoded] = fieldData.fi.fieldTypeName match {
      case "UInt16" if fieldName === "LedgerEntryType" => MiscCodecs.encodeLedgerEntryType(fieldValue)
      case "UInt16" if fieldName === "TransactionType" => MiscCodecs.encodeTransactionType(fieldValue)

      case "AccountID" => AccountIdCodecs.encodeAccount(fieldValue)
      case "UInt8"     => UIntCodecs.encodeUInt8(fieldValue)
      case "UInt16"    => UIntCodecs.encodeUInt16(fieldValue)
      case "UInt32"    => UIntCodecs.encodeUInt32(fieldValue)
      case "UInt64"    => UIntCodecs.encodeUInt64(fieldValue)
      case "Hash160"   => HashHexCodecs.encodeHash160(fieldValue)
      case "Hash256"   => HashHexCodecs.encodeHash256(fieldValue)
      case "Blob"      => MiscCodecs.encodeBlob(fieldValue)
      case "Amount"    => MoneyCodecs.encodeAmount(fieldValue)
      case "PathSet"   => PathCodecs.encodePathSet(fieldValue)
      case "Vector256" => ContainerFields.encodeVector256(fieldValue)
      case "STArray"   => ContainerFields.encodeSTArray(fieldValue, signingModeOn)
      case "STObject"  => ContainerFields.encodeSTObject(fieldValue, signingModeOn)

      case other => BinCodecLibError(s"Not handling Field Type $other").asLeft

    }

    // Lets drag some baggage along, the original Json and FieldMetaData for debugging
    val full: Either[BinCodecLibError, EncodedField] = valueBytes.map(EncodedField(_, fieldData))
    full

  }

}
