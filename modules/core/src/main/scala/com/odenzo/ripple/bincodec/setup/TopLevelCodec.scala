package com.odenzo.ripple.bincodec.setup

import com.odenzo.ripple.bincodec.scodecs.FieldScodec
import io.circe.{Json, JsonObject}
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector

/** Top Level Decoding of a full message, expecting to be an STObject with no marker */
object DecoderController {

  val _ = Setup.config // Just to trigger all the logger

  def decode(hex: String): DecodeResult[JsonObject] = {
    val binary: BitVector = BitVector.fromHex(hex).getOrElse(throw new Exception("Invalid Input Hex"))
    decode(binary)
  }

  def decode(bv: BitVector): DecodeResult[JsonObject] = {
    scribe.debug(s"Decoding Top Vector of Fields from ${bv.size}")
    val result = scodec.codecs.vector(FieldScodec.xrpfield).decode(bv).require
    result.map(JsonObject.fromIterable)
  }
}
