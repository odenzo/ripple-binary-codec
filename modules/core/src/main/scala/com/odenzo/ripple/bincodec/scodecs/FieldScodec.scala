package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.config.FieldEntry
import com.odenzo.ripple.bincodec.scodecs
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder}
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
  val xrpfieldDecoder: Decoder[(String, Json)] = scodec.Decoder(decodeNextField _)
  val xrpfieldEncoder: Encoder[(String, Json)] = scodec.Encoder[(String, Json)]((x: (String, Json)) => Attempt.successful(BitVector.empty))
  val xrpfield: Codec[(String, Json)]          = Codec(xrpfieldEncoder, xrpfieldDecoder).withContext("Field Decoder")

  protected def decodeNextField(bv: BitVector): Attempt[DecodeResult[(String, Json)]] = {
    scribe.debug(
      s"Decoding  Field from ${bv.size} bits \n " +
        s"100 Hex ${bv.take(100 * 4).toHex}"
    )
    val fieldtypecodesResult = FieldIdScodec.xrpfieldid.decode(bv).require
    val remainingBytes       = fieldtypecodesResult.remainder
    val backToHex            = FieldIdScodec.xrpfieldid.encode(fieldtypecodesResult.value).require.toHex

    scribe.debug(
      s"FieldID: ${fieldtypecodesResult.value}  Hex: ${backToHex}  Remaining: ${remainingBytes.size}"
    )
    val fieldEncriched    = Setup.findFieldByFieldId(fieldtypecodesResult.value)
    val field: FieldEntry = fieldEncriched._1
    scribe.debug(s"Found Field: ${pprint.apply(fieldEncriched._1)} }")
    val typeName    = field.metadata.typeName
    val isVlEncoded = field.metadata.isVLEncoded
    scribe.debug(s"Is VL Encoded: $isVlEncoded")

    val result: Attempt[DecodeResult[Json]] = if (isVlEncoded && typeName == "AccountID") {
      ScodecDataTypeBinding.dynamicDecode(fieldtypecodesResult.remainder, "AccountIDVL")
    } else {
      ScodecDataTypeBinding.dynamicDecode(fieldtypecodesResult.remainder, typeName)
    }

    result.map(dr => DecodeResult(field.name -> dr.value, dr.remainder))

  }

  protected def encodeNextField(fieldName: String, content: Json) = {
    val (fieldEntry, fieldId) = Setup.findFieldByName(fieldName)

    val id = FieldIdScodec.xrpfieldid.encode((fieldId.fieldCode, fieldId.typeCode))

    // Okay, now we try and encode the content (which may be a sub-object, array or whatever
    fieldEntry.metadata.typeName

  }
}
object FieldScodec extends FieldScodec
