package com.odenzo.ripple.bincodec

import com.typesafe.scalalogging.StrictLogging
import io.circe.JsonObject
import io.circe.syntax._

import com.odenzo.ripple.bincodec.serializing.{BinarySerializer, TypeSerializers}
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError

object RippleCodecAPI extends StrictLogging {

  /**
    * Expects a top level JsonObject representing a JSON document
    * that would be sent to rippled server. All isSerializable fields serialized.
    *
    * @param jsonObject
    *
    * @return Hex string representing the serialization in total.
    */
  def binarySerialize(jsonObject: JsonObject): Either[RippleCodecError, BinarySerializer.NestedEncodedValues] = {
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
  def binarySerializeForSigning(tx_json: JsonObject): Either[RippleCodecError, BinarySerializer.NestedEncodedValues] = {
    logger.trace("Serializing for Signing")
    TypeSerializers.encodeTopLevel(tx_json.asJson, isSigning = true)
  }

  /**
    * Binary Serialize the tx_json with all fields marked "isSerialized" per Ripple Spec
    *
    * @param jsonObject The tx_json object, with auto-fillable fields filled
    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
    */
  def serializedTxBlob(jsonObject: JsonObject): Either[RippleCodecError, Array[Byte]] = {
    binarySerialize(jsonObject).map(_.rawBytes).map(v ⇒ v.map(_.toByte)).map(_.toArray)
  }

  /**
    * Binary Serialize the tx_json with all fields marked "isSigningField" per Ripple Spec
    *
    * @param jsonObject The tx_json object, with auto-fillable fields filled
    *
    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
    */
  def signingTxBlob(jsonObject: JsonObject): Either[RippleCodecError, Array[Byte]] = {
    binarySerializeForSigning(jsonObject).map(v⇒ v.toBytes)
  }

}
