package com.odenzo.ripple.bincodec

import com.typesafe.scalalogging.StrictLogging
import io.circe.JsonObject
import io.circe.syntax._

import com.odenzo.ripple.bincodec.serializing.{BinarySerializer, TypeSerializers}
import com.odenzo.ripple.bincodec.utils.caterrors.AppError

object RippleLocalAPI extends StrictLogging {


  /**
    * Expects a top level JsonObject representing a JSON document
    * that would be sent to rippled server. All isSerializable fields serialized.
    *
    * @param jsonObject
    *
    * @return Hex string representing the serialization in total.
    */
  def binarySerialize(jsonObject: JsonObject): Either[AppError, BinarySerializer.NestedEncodedValues] = {
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
  def binarySerializeForSigning(tx_json: JsonObject): Either[AppError, BinarySerializer.NestedEncodedValues] = {
    logger.trace("Serializing for Signing")
    TypeSerializers.encodeTopLevel(tx_json.asJson, isSigning = true)

  }


}
