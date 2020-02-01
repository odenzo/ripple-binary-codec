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
  val xrpfield: Codec[(Json, Json)]                 = Codec(xrpfieldEncoder, xrpfieldDecoder).withContext("Field Decoder")

  def decodeNextField(bv: BitVector): Attempt[DecodeResult[(Json, Json)]] = {
    scribe.debug(
      s"Decoding  Field from ${bv.size} bits \n " +
        s"100 Hex ${bv.take(100 * 4).toHex}"
    )
    val fieldtypecodesResult: DecodeResult[(FieldCode, TypeCode)] = FieldIdScodec.xrpfieldid.decode(bv).require
    val backToHex                                                 = FieldIdScodec.xrpfieldid.encode(fieldtypecodesResult.value).require.toHex
    val fieldIdLen                                                = bv.length - fieldtypecodesResult.remainder.length

    scribe.debug(
      s"FieldID: ${fieldtypecodesResult.value}  Hex: ${backToHex}  Size: ${fieldIdLen} Remaining: ${fieldtypecodesResult.remainder.size}"
    )
    val fieldEncriched = Setup.findFieldByFieldId(fieldtypecodesResult.value)
    val field          = fieldEncriched._1
    scribe.debug(s"Found Field: ${pprint.apply(fieldEncriched._1)} }")
    val typeName    = field.metadata.typeName
    val isVlEncoded = field.metadata.isVLEncoded
    scribe.debug(s"Is VL Encoded: $isVlEncoded")

    for {
      result <- if (isVlEncoded && typeName == "AccountID") {
        ScodecDataTypeBinding.dynamicDecode(fieldtypecodesResult.remainder, "AccountIDVL")
      } else {
        ScodecDataTypeBinding.dynamicDecode(fieldtypecodesResult.remainder, typeName)
      }
      decRes: DecodeResult[(Json, Json)] = DecodeResult(field.name.asJson -> result.value, result.remainder)
      _                                  = scribe.info(s"Decoded Json: ${decRes.value.asJson.spaces4}")
    } yield decRes

  }
}
object FieldScodec extends FieldScodec
