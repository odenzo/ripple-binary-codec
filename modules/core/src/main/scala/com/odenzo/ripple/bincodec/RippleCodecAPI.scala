package com.odenzo.ripple.bincodec

import io.circe.Json

import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.utils.JsonUtils

/**
  * This will fail on fields which have a null value.
  *
  */
object RippleCodecAPI {

  //scribe.warn("RippleCodeAPI Setup")
  //com.odenzo.ripple.bincodec.defaultSetup()

//  private def encode(json: Json, forSign: Boolean): Either[BinCodecLibError, Array[Byte]] = {
//    TypeSerializers.encodeTopLevel(json, isSigning = false)
//  }
//
//  def parseJson(json: String): Either[BinCodecLibError, Json] = JsonUtils.parseAsJson(json)
//
//  /**
//    * Binary Serialize the tx_json with all fields marked "isSerialized" per Ripple Spec
//    *
//    * @param jsonObject The tx_json object, with auto-fillable fields filled
//    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
//    */
//  def serializedTxBlob(jsonObject: Json): Either[BinCodecLibError, Array[Byte]] = {
//    encode(jsonObject, forSign = false)
//  }
//
//  /**
//    * Binary Serialize the tx_json with all fields marked "isSigningField" per Ripple Spec
//    *
//    * @param jsonObject The tx_json object, with auto-fillable fields filled
//    *
//    * @return Binary format, equivalent to tx_blob when converted to Hex, no padding needed
//    */
//  def signingTxBlob(jsonObject: Json): Either[BinCodecLibError, Array[Byte]] = {
//    encode(jsonObject, forSign = true)
//  }
//
//  /**
//    * When doing multisigning, the signing address is appended (suffix) to the serialized for signing bytes
//    * as part of the process to form the payload for signing. This can be used for that. Rather simple now and
//    * just converts Base58Check to bytes, but we go through the main path to do so.
//    *
//    * @param addressBase58Check An account address in Base58Check form, e.g.  r9EP7xcWBAWEHhgtm4evqsHTJT4ZesJHXX
//    * @return Array of bytes or an error. This currently is NOT VLEncoded, just raw bytes.
//    */
//  def serializedAddress(addressBase58Check: String): Either[BinCodecLibError, Array[Byte]] = {
  // AccountIdCodecs.encodeAccountNoVL(addressBase58Check).map(x => x.toArray)
  //}
}
