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
    ContainerFields.encodeSTObject(json, isNested = false, isSigning = isSigning)
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
    * @param isNestedObject Not sure we actually need this.
    * @param signingModeOn True if we are encoding for signing vs full serialization
    * @return
    */
  def encodeFieldAndValue(
      fieldData: FieldData,
      isNestedObject: Boolean,
      signingModeOn: Boolean
  ): Either[BinCodecLibError, EncodedField] = {

    val fieldName: String = fieldData.fieldName
    val fieldValue: Json  = fieldData.json

    scribe.debug(s"Encoding FieldValue: $fieldData")

    val valueBytes: Either[BinCodecLibError, Encoded] = fieldData.fi.fieldTypeName match {
      case "UInt16" if fieldName === "LedgerEntryType" => MiscCodecs.encodeLedgerEntryType(fieldValue)
      case "UInt16" if fieldName === "TransactionType" => MiscCodecs.encodeTransactionType(fieldValue)

      // I tihnk this is the only case I use nested. The meaning is really if its packed in (like FiatAmount)
      // not in a JsonObject as plain field below the to object
      case "AccountID" if isNestedObject  => AccountIdCodecs.encodeAccount(fieldValue)
      case "AccountID" if !isNestedObject => AccountIdCodecs.encodeAccount(fieldValue)

      case "UInt8"     => UIntCodecs.encodeUIntN(fieldValue, "UInt8")
      case "UInt16"    => UIntCodecs.encodeUIntN(fieldValue, "UInt16")
      case "UInt32"    => UIntCodecs.encodeUIntN(fieldValue, "UInt32")
      case "UInt64"    => UIntCodecs.encodeUInt64(fieldValue)
      case "Hash160"   => HashHexCodecs.encodeHash160(fieldValue)
      case "Hash256"   => HashHexCodecs.encodeHash256(fieldValue)
      case "Blob"      => MiscCodecs.encodeBlob(fieldValue)
      case "Amount"    => MoneyCodecs.encodeAmount(fieldValue)
      case "PathSet"   => PathCodecs.encodePathSet(fieldData)
      case "Vector256" => ContainerFields.encodeVector256(fieldData)
      case "STArray"   => ContainerFields.encodeSTArray(fieldData, signingModeOn)
      case "STObject"  => ContainerFields.encodeSTObject(fieldValue, isNestedObject, signingModeOn)

      case other => BinCodecLibError(s"Not handling Field Type $other").asLeft

    }

    // Lets drag some baggage along, the original Json and FieldMetaData for debugging
    val full: Either[BinCodecLibError, EncodedField] = valueBytes.map(EncodedField(_, fieldData))
    full

  }

}
