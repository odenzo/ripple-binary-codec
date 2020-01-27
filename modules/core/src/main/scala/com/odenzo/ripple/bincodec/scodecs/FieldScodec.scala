package com.odenzo.ripple.bincodec.scodecs

import scodec.{Attempt, Codec, DecodeResult}

import com.odenzo.ripple.bincodec.scodecs.FieldIdScodec.{FieldCode, TypeCode}
import scodec.bits.BitVector

import com.odenzo.ripple.bincodec.setup.{ScodecDataTypeBinding, Setup}
import io.circe.Json
import io.circe.syntax._

trait FieldScodec {

  // Need to checkout the other branch, the top level txn thing doesn't have a field, just a STObject with no end marker.
  // So, the top driver is simply  decoderNextField  while bitvector.is not empty

  /** Start of xrpfieldcodec. The decoding part. The encoder will have to have JSON with current approach
    * I want to try and see if I can write it to easily replace the model with existing case classes later.
    * Field Name should be procomputed JSON
    * */
  val xrpfieldDecoder: scodec.Decoder[(Json, Json)] = scodec.Decoder(decodeNextField _)
  val xrpfieldEncoder: scodec.Encoder[(Json, Json)] = scodec.Encoder[(Json, Json)]((x: (Json, Json)) => Attempt.successful(BitVector.empty))
  val xrpfield: Codec[(Json, Json)]                 = Codec(xrpfieldEncoder, xrpfieldDecoder)

  def decodeNextField(bv: BitVector): Attempt[DecodeResult[(Json, Json)]] = {
    scribe.debug(s"Decoding  Field from ${bv.size} bits, first 3 hex ${bv.take(3 * 8).toHex}")
    val fieldtypecodesResult: DecodeResult[(FieldCode, TypeCode)] = FieldIdScodec.xrpfieldid.decode(bv).require
    val backToHex                                                 = FieldIdScodec.xrpfieldid.encode(fieldtypecodesResult.value).require.toHex
    scribe.debug(s"FieldID: ${fieldtypecodesResult.value}  Hex: ${backToHex}  Remaining: ${fieldtypecodesResult.remainder.size}")
    val fieldEncriched = Setup.findFieldByFieldId(fieldtypecodesResult.value)
    val field          = fieldEncriched._1
    scribe.debug(s"Found Field: ${pprint.apply(fieldEncriched._1)} }")
    val typeName    = field.metadata.typeName
    val isVlEncoded = field.metadata.isVLEncoded

    if (isVlEncoded) {
      import scodec.codecs._
      // fixedSizeBits()
      scribe.debug(s"Is VL Encoded: $isVlEncoded")
      for {
        vl <- VL.xrpvl.decode(fieldtypecodesResult.remainder)
        prealloc = bv.splitAt(vl.value * 8)
        result <- ScodecDataTypeBinding.dynamicDecode(prealloc._1, typeName)
        decRes: DecodeResult[(Json, Json)] = DecodeResult(field.name.asJson -> result.value, prealloc._2)
        _                                  = scribe.info(s"Decoded Json: ${decRes.value.asJson.spaces4}")
      } yield decRes
      // Careful about double VL encoding. xrp*
      // scodecs should use combinators if not calling from top of a field

    } else {
      import cats._
      import cats.data._
      import cats.implicits._
      for {
        result <- ScodecDataTypeBinding.dynamicDecode(fieldtypecodesResult.remainder, typeName)
        _   = scribe.debug(s"Dynamic Decode $typeName : $result")
        enc = result.map(fv => field.name.asJson -> fv)
        _   = scribe.info(s"Decoded Json: ${enc.value.asJson.noSpaces}")
      } yield enc
    }
  }
}
object FieldScodec extends FieldScodec
