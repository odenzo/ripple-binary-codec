package com.odenzo.ripple.bincodec.encoding

import cats.data._
import cats.implicits._
import io.circe.Json

import com.odenzo.ripple.bincodec.codecs._
import com.odenzo.ripple.bincodec.reference.FieldData
import com.odenzo.ripple.bincodec.BinCodecLibError

/**
  *  Goes throught and deep serializes a JsonObject
  */
object TypeSerializers extends CodecUtils {

  import scodec.bits.ByteVector

  import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr

  /** The very top level object, which doesn't get an end of object marker */
  def encodeTopLevel(json: Json, isSigning: Boolean): Either[BinCodecLibError, Array[Byte]] = {
    scribe.debug(s"Encoding TopLevel ${json.spaces4}")
    // I would like to do a deep filtering here but not doing yet
    val prepped = ContainerFields.prepareJsonObject(json, isSigning)
    ContainerFields.encodeObject(json, isSigning).map(bv => bv.toArray)

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
  def encodeFieldAndValue(fieldData: FieldData, signingModeOn: Boolean): ErrorOr[ByteVector] = {
    scribe.debug(s"Encoding FieldValue: $fieldData")
    val fname: String = fieldData.fieldName
    val fv: Json      = fieldData.json

    // Could bind the encoder to fieldData but I think this way is clearer

    val valueBytes = fieldData.fi.fieldTypeName match {
      case "UInt16" if fname === "LedgerEntryType" => json2string(fv) >>= MiscCodecs.encodeLedgerEntryType
      case "UInt16" if fname === "TransactionType" => json2string(fv) >>= MiscCodecs.encodeTransactionType

      case "UInt8"   => json2long(fv) >>= VLEncodingOldBroken.encodeUInt8
      case "UInt16"  => json2long(fv) >>= VLEncodingOldBroken.encodeUInt16
      case "UInt32"  => json2long(fv) >>= VLEncodingOldBroken.encodeUInt32
      case "UInt64"  => json2bigint(fv) >>= VLEncodingOldBroken.encodeUInt64
      case "Hash160" => json2string(fv) >>= VLEncodingOldBroken.encodeHash160
      case "Hash256" => json2string(fv) >>= VLEncodingOldBroken.encodeHash256
      case "Blob"    => json2string(fv) >>= VLEncodingOldBroken.encodeBlob

      case "Vector256" => JsonCodecs.encodeVector256(fv)
      case "STArray"   => JsonCodecs.encodeSTArray(fv, signingModeOn)
      case "STObject"  => ContainerFields.encodeSTObject(fv, signingModeOn)

      case "AccountID" => json2string(fv) >>= AccountIdCodecs.encodeAccount
      case "Amount"    => MoneyCodecs.encodeAmount(fv)
      case "PathSet"   => PathCodecs.encodePathSet(fv)

      case other => throw BinCodecLibError(s"Not handling Field Type $other")

    }

    valueBytes

  }

}
