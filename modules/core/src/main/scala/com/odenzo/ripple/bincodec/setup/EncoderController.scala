package com.odenzo.ripple.bincodec.setup

import com.odenzo.ripple.bincodec.config.FieldMeta
import com.odenzo.ripple.bincodec.scodecs.FieldScodec
import io.circe.JsonObject
import io.circe.syntax._
import scodec.bits.BitVector

/** Top Level Decoding of a full message, expecting to be an STObject with no marker */
object EncoderController {

  val _ = Setup.config // Just to trigger all the logger

  /** Encodes a complete JSON Object, typically the tx_json of a transaction */
  def encode(json: JsonObject, forSigning: Boolean): BitVector = {

    // We encode the fields one-by-one since no STObject End Marker is used
    scribe.debug(s"Encoding For Signing: $forSigning JSON Object: ${json.asJson.spaces4}")

    val fields = preprocessJson(forSigning, json).toVector

    val result: BitVector = scodec.codecs.vector(FieldScodec.xrpfield).encode(fields).require
    scribe.info(s"Encoded Result: ${result.toHex}")
    result
  }

  /** Quick top level only hack. Breaks the concept of pre-normalizing
    * the transaction type.  */
  def preprocessJson(forSigning: Boolean, jobj: JsonObject): JsonObject = {
    /* What is this about, well, we need to sort all objects by fieldID of the field name.
       Then we need to filter the fields based on if they are signing/serialized (or neither)
      Actually not all, just STObjects? Hmm, this may get tricky.
      STObjects can be nested in STArray etc, underderstanding content can I think
      just limit to STObject in STObject so just traversing the JsonObject(s)?
      Look at new Circe traversals.
     */
    /* jo.toList.sortBy { // FieldID
      case (fname, _) => Setup.findFieldByName(fname)._2.bv
    }

     */
    // There are a few  fields in definition that are not signing or serialized fields
    def filterSigning(fm: FieldMeta) = fm.isSigningField

    def filterSerializing(fm: FieldMeta) = fm.isSerialized

    val filterFn = if (forSigning) filterSigning _ else filterSerializing _

    val sortedAndFiltersTop = jobj
      .filterKeys(fname => filterFn(Setup.findFieldByName(fname)._1.metadata))
      .toVector
      .sortBy { case (fname, content) => Setup.findFieldByName(fname)._2.orderKey }

    JsonObject.fromIterable(sortedAndFiltersTop)
  }
}
