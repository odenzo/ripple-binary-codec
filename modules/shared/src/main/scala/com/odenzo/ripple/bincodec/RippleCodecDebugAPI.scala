package com.odenzo.ripple.bincodec

import io.circe.Json

import com.odenzo.ripple.bincodec.codecs.AccountIdCodecs
import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.utils.JsonUtils

/**
  * This will fail on fields which have a null value.
  *
  */
object RippleCodecDebugAPI {

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
  def binarySerialize(jsonObject: Json): Either[BinCodecLibError, EncodedSTObject] = {
    TypeSerializers.encodeTopLevel(jsonObject, isSigning = false)
  }

  /**
    * Expects a top level JsonObject representing a transaction
    * that would be sent to rippled server. All isSigningField fields serialized.
    *
    * This does not populate any "auto-fill" fields
    *
    * @param tx_json
    */
  def binarySerializeForSigning(tx_json: Json): Either[BinCodecLibError, EncodedSTObject] = {
    TypeSerializers.encodeTopLevel(tx_json, isSigning = true)
  }

}
