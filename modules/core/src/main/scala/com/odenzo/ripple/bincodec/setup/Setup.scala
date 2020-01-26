package com.odenzo.ripple.bincodec.setup

import io.circe.{Encoder, Json}
import scodec.{Attempt, Codec, Decoder, DecodeResult}
import scodec.bits.BitVector
import spire.math._
import spire.implicits._

import com.odenzo.ripple.bincodec.config.{FieldEntry, RippleConfig}
import com.odenzo.ripple.bincodec.scodecs.{FieldIdScodec, FieldScodec, TrivialScodec, VL}
import com.odenzo.ripple.bincodec.scodecs.FieldIdScodec.{FieldCode, TypeCode}
import io.circe.syntax._
import scodec.bits.Bases.Alphabets.HexUppercase

object Setup {

  //<editor-fold desc="Data Modelling Zone that will Need to be Optimized">
  val config: RippleConfig = RippleConfig.loadFromDefaultFile().fold(e => throw e, identity)

  scribe.info(s"Raw Data Types: ${pprint.apply(config.types)}")

  val dataTypes: Map[String, Int] = config.types.toList.filter(v => v._2 >= 0).toMap
  val fields: List[FieldEntry]    = config.fields.filter(v => (v.metadata.nth > 0) && (v.metadata.nth < 256))

  scribe.info(s"Data Types: ${pprint.apply(dataTypes)}")

  val datatypeCode2datatypeNameMap: Map[Int, String] = dataTypes
    .toList
    .map(x => (x._2, x._1))
    .toMap

  def bindFieldIdToFields(): Unit = {
    import cats._
    import cats.data._
    import cats.implicits._
    import cats._
    import cats.data._
    import cats.implicits._

    scribe.trace(s"binding fieldids to fields")

    fields.fproduct { fe =>
      scribe.info(s"binding field ${pprint.apply(fe)}")
      val typename    = fe.metadata.typeName
      val typecode    = dataTypes(typename)
      val typecodeAdj = if (typecode > 1000) UInt(1) else UInt(typecode) // Transaction, LedgerEntry, etc.
      val fieldcode   = UInt(fe.metadata.nth)
      // scribe.info(s"FieldCode: $fieldcode  TypeCode: $typecodeAdj")
      val encodedFielduid = FieldIdScodec.xrpfieldid.encode((fieldcode, typecodeAdj)).require
      scribe.info(s"FieldCode: $fieldcode  TypeCode: $typecodeAdj \t Encoded: ${encodedFielduid.toHex(HexUppercase)}")
      encodedFielduid
    }
  }

  //</editor-fold>

  /** What do I really want... since vals. I need a fieldMarker <-> scodec for sure */
  /*
  + Decoder fieldcode and typecode  from endless BitVector for fieldid (done). Maybe nice to get the BitVector consumed (?)
  + Find the Data Type for the field and decode it. Repeat ad-naseum.
  + Decoding goes to JSON then we need to rhw field, to get the name. And also need to know how to JSON encode it. Atoms and Containers.
  + The data type should mostly tell how to encode actually. Don't forget about is VLEncoded on the decoding side. Bummer.
   * Now we just need the datatypeName so just need to quickly get the field record
   */

  def datetypeCodeToName(datatypecode: Int): String = {
    datatypeCode2datatypeNameMap(datatypecode)
  }
  // Decoder thoughts:
  // Given full bitvector:
  // DecodeFieldId map DataTypeId match to decoder apply decoding return dependantType (Any) or decode to ADT model
  // or decode directly to JSON (?), that sounds good actually.

  //def bindScodecs(types: Map[String, Int], binding: Map[String, Codec[Nothing]]) = {}
}

/** Top Level Decoding of a full message, expecting to be an STObject with no marker */
object DecoderController {

  def decode(hex: String) = {
    val binary: BitVector = BitVector.fromHex(hex).getOrElse(throw new Exception("Invalid Input Hex"))
    scodec.codecs.vector(FieldScodec.xrpfield)
  }

}
