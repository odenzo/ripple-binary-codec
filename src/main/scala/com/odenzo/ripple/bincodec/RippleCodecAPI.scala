package com.odenzo.ripple.bincodec

import io.circe.JsonObject
import io.circe.syntax._

import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError

object RippleCodecAPI  {

  /**
    * Expects a top level JsonObject representing a JSON document
    * that would be sent to rippled server. All isSerializable fields serialized.
    *
    * @param jsonObject
    *
    * @return Hex string representing the serialization in total.
    */
  def binarySerialize(jsonObject: JsonObject): Either[RippleCodecError, EncodedNestedVals] = {
    TypeSerializers.encodeTopLevel(jsonObject.asJson, isSigning = false)
  }

  /**
    * Expects a top level JsonObject representing a transaction
    * that would be sent to rippled server. All isSigningField fields serialized.
    * This and binarySerialize and the only two top level user
    * FIXME: I am guessing this is the whole transaction because fee_multi_max and other important stuff in top
    * level
    *
    * @param tx_json
    */
  def binarySerializeForSigning(tx_json: JsonObject): Either[RippleCodecError, EncodedNestedVals] = {
    TypeSerializers.encodeTopLevel(tx_json.asJson, isSigning = true)
  }

  /**
    * Binary Serialize the tx_json with all fields marked "isSerialized" per Ripple Spec
    *
    * @param jsonObject The tx_json object, with auto-fillable fields filled
    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
    */
  def serializedTxBlob(jsonObject: JsonObject): Either[RippleCodecError, Array[Byte]] = {
    binarySerialize(jsonObject).map(_.toBytes)
  }

  /**
    * Binary Serialize the tx_json with all fields marked "isSigningField" per Ripple Spec
    *
    * @param jsonObject The tx_json object, with auto-fillable fields filled
    *
    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
    */
  def signingTxBlob(jsonObject: JsonObject): Either[RippleCodecError, Array[Byte]] = {
    binarySerializeForSigning(jsonObject).map(_.toBytes)
  }


  /**
    * Binary Serialize the tx_json with all fields marked "isSerialized" per Ripple Spec
    *
    * @param  tx tx_json object, with auto-fillable fields filled, as String to decouple JSON impl
    *
    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
    */
  def serializedTxBlob(tx: String): Either[RippleCodecError, Array[Byte]] = {
     JsonUtils.parseAsJsonObject(tx).flatMap(serializedTxBlob) }

  /**
    * Binary Serialize the tx_json with all fields marked "isSigningField" per Ripple Spec
    *
    * @param tx The tx_json object, with auto-fillable fields filled, as String to decouple JSON impl
    *
    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
    */
  def signingTxBlob(tx: String): Either[RippleCodecError, Array[Byte]] = {
    JsonUtils.parseAsJsonObject(tx).flatMap(signingTxBlob)
  }
}
