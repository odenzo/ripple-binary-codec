package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.setup.{ScodecDataTypeBinding, Setup}
import io.circe.Json
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder}

trait FieldScodec {

  /** Start of xrpfieldcodec. The decoding part. The encoder will have to have JSON with current approach
    * I want to try and see if I can write it to easily replace the model with existing case classes later.
    * Field Name should be procomputed JSON
    * */
  val xrpfieldDecoder: Decoder[(String, Json)] = scodec.Decoder(decodeNextField _)
  val xrpfieldEncoder: Encoder[(String, Json)] = scodec.Encoder[(String, Json)](encodeNextField _)
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
    val (fieldEntry, fieldId) = Setup.findFieldByFieldId(fieldtypecodesResult.value)
    scribe.debug(s"Found Field: ${pprint.apply(fieldEntry)} }")
    val typeName    = fieldEntry.metadata.typeName
    val isVlEncoded = fieldEntry.metadata.isVLEncoded
    scribe.debug(s"Is VL Encoded: $isVlEncoded")

    val result: Attempt[DecodeResult[Json]] = if (isVlEncoded && typeName == "AccountID") {
      ScodecDataTypeBinding.dynamicDecode(fieldtypecodesResult.remainder, "AccountIdVL")
    } else {
      ScodecDataTypeBinding.dynamicDecode(fieldtypecodesResult.remainder, typeName)
    }

    result.map(dr => DecodeResult(fieldEntry.name -> dr.value, dr.remainder))

  }

  // @todo Need to know if signing or serializing here for encoding, or do at STObject level?
  // Or pre-processed all the JSON ahead of time (preferred)
  protected def encodeNextField(tuple: (String, Json)): Attempt[BitVector] = {
    val (fieldName, content)  = tuple
    val (fieldEntry, fieldId) = Setup.findFieldByName(fieldName)

    scribe.debug(s"Field ENtry: $fieldEntry")
    FieldIdScodec.xrpfieldid.encode(fieldId).flatMap { id =>
      // Quick Hack... I think all accounts will be VL Encoded if they are a field
      // Nested will be encoded through the enclosing encoder directly call accountNoVL
      val typeName =
        if (fieldEntry.name == "TransactionType") "Transaction"
        else if (fieldEntry.metadata.typeName == "AccountID" && fieldEntry.metadata.isVLEncoded) "AccountIdVL"
        else if (fieldEntry.metadata.typeName == "AccountID" && !fieldEntry.metadata.isVLEncoded) "AccountIdNoVL"
        else fieldEntry.metadata.typeName
      scribe.debug(s"Encoding $typeName")
      val body = ScodecDataTypeBinding.dynamicEncode(content, typeName)
      body.map(b => id ++ b)
    }

  }
}
object FieldScodec extends FieldScodec
