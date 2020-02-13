package com.odenzo.ripple.bincodec.setup

import com.odenzo.ripple.bincodec.scodecs.FieldScodec
import io.circe.Json
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector

/** Top Level Decoding of a full message, expecting to be an STObject with no marker */
object DecoderController {

  val _ = Setup.config // Just to trigger all the logger

  def decode(hex: String): Attempt[DecodeResult[Vector[(Json, Json)]]] = {
    val binary: BitVector = BitVector.fromHex(hex).getOrElse(throw new Exception("Invalid Input Hex"))
    decode(binary)
  }

  def decode(bv: BitVector): Attempt[DecodeResult[Vector[(Json, Json)]]] = {
    scribe.debug(s"Decoding Top Vector of Fields from ${bv.size}")
    scodec.codecs.vector(FieldScodec.xrpfield).decode(bv)
  }
}
