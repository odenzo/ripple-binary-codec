package com.odenzo.ripple.bincodec

import io.circe.{Json, JsonObject}
import io.circe.syntax._
import scribe.Level.Debug

import com.odenzo.ripple.bincodec.codecs.AccountIdCodecs
import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError

object RippleCodecAPI {

  //scribe.warn("RippleCodeAPI Setup")
  //com.odenzo.ripple.bincodec.defaultSetup()

  /**
    * Expects a top level JsonObject representing a JSON document
    * that would be sent to rippled server. All isSerializable fields serialized.
    *
    * @param jsonObject
    *
    * @return Hex string representing the serialization in total.
    */
  def binarySerialize(jsonObject: JsonObject): Either[RippleCodecError, EncodedSTObject] = {
    TypeSerializers.encodeTopLevel(jsonObject.asJson, isSigning = false)
  }

  /**
    * Expects a top level JsonObject representing a transaction
    * that would be sent to rippled server. All isSigningField fields serialized.
    *
    * This does not populate any "auto-fill" fields
    *
    * @param tx_json
    */
  def binarySerializeForSigning(tx_json: JsonObject): Either[RippleCodecError, EncodedSTObject] = {
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
    JsonUtils.parseAsJsonObject(tx).flatMap(serializedTxBlob)
  }

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

  /**
    * When doing multisigning, the signing address is appended (suffix) to the serialized for signing bytes
    * as part of the process to form the payload for signing. This can be used for that. Rather simple now and
    * just converts Base58Check to bytes, but we go through the main path to do so.
    *
    * @param addressBase58Check An account address in Base58Check form, e.g.  r9EP7xcWBAWEHhgtm4evqsHTJT4ZesJHXX
    * @return Array of bytes or an error. This currently is NOT VLEncoded, just raw bytes.
    */
  def serializedAddress(addressBase58Check: String): Either[RippleCodecError, Array[Byte]] = {
    AccountIdCodecs.encodeAccountNoVL(Json.fromString(addressBase58Check)).map(_.toBytes)
  }
}
