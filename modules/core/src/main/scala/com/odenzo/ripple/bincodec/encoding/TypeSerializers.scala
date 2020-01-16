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

  import com.odenzo.ripple.bincodec.codecs.ScodecStyleCodecs._

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
    scribe.debug(s"Encoding FieldValue: $fieldData")

    val fname: String = fieldData.fieldName
    val fv: Json      = fieldData.json

    // Could bind the encoder to fieldData but I think this way is clearer

    val valueBytes: Either[BinCodecLibError, Encoded] = fieldData.fi.fieldTypeName match {
      case "UInt16" if fname === "LedgerEntryType" => json2string(fv) >>= MiscCodecs.encodeLedgerEntryType
      case "UInt16" if fname === "TransactionType" => json2string(fv) >>= MiscCodecs.encodeTransactionType
      case "AccountID"                             => AccountIdCodecs.encodeAccount(fv)
      case "UInt8"                                 => json2long(fv) >>= (scodecUInt8 _)
      case "UInt16"                                => json2long(fv) >>= (scodecUInt16 _)
      case "UInt32"                                => json2long(fv) >>= (scodecUInt32 _)
      case "UInt64"                                => json2bigint(fv) >>= (scodecUInt64 _)
      case "Hash160"                               => json2string(fv) >>= HashHexCodecs.encodeHash160
      case "Hash256"                               => json2string(fv) >>= HashHexCodecs.encodeHash256
      case "Blob"                                  => json2string(fv) >>= MiscCodecs.encodeBlob
      case "Amount"                                => MoneyCodecs.encodeAmount(fv)
      case "PathSet"                               => PathCodecs.encodePathSet(fv)
      case "Vector256"                             => ContainerFields.encodeVector256(fv)
      case "STArray"                               => ContainerFields.encodeSTArray(fv, signingModeOn)
      case "STObject"                              => ContainerFields.encodeSTObject(fv, signingModeOn)

      case other => BinCodecLibError(s"Not handling Field Type $other").asLeft

    }

    // Lets drag some baggage along, the original Json and FieldMetaData for debugging
    val full: Either[BinCodecLibError, EncodedField] = valueBytes.map(EncodedField(_, fieldData))
    full

  }

}
