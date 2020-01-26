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
  val xrpfieldEncoder: scodec.Encoder[(Json, Json)] = scodec.Encoder[(Json, Json)](_ => Attempt.successful(BitVector.empty))
  val xrpfield                                      = Codec(xrpfieldEncoder, xrpfieldDecoder)

  def decodeNextField(bv: BitVector): Attempt[DecodeResult[(Json, Json)]] = {
    val idMaybe: DecodeResult[(FieldCode, TypeCode)] = FieldIdScodec.xrpfieldid.decode(bv).require
    val (fc, tc: TypeCode)                           = idMaybe.value
    val field = Setup
      .config
      .fields
      .find(_.metadata.nth == fc.toInt)
      .getOrElse(throw new Exception(s"Missing Field $fc"))
    val typeName    = field.metadata.typeName
    val isVlEncoded = field.metadata.isVLEncoded
    if (isVlEncoded) {
      import scodec.codecs._
      // fixedSizeBits()
      for {
        vl <- VL.xrpvl.decode(bv)
        prealloc = bv.splitAt(vl.value * 8)
        result <- ScodecDataTypeBinding.dynamicDecode(prealloc._1, typeName)
      } yield DecodeResult(field.name.asJson -> result.value, prealloc._2)
      // Careful about double VL encoding. xrp*
      // scodecs should use combinators if not calling from top of a field

    } else {
      ScodecDataTypeBinding
        .dynamicDecode(bv, typeName)
        .map(result => result.map(fv => field.name.asJson -> fv))
    }
  }
}
object FieldScodec extends FieldScodec
