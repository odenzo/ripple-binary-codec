package com.odenzo.ripple.bincodec.setup

import io.circe.Json
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import spire.implicits._

import com.odenzo.ripple.bincodec.config.{FieldEntry, RippleConfig}
import com.odenzo.ripple.bincodec.scodecs.{FieldIdScodec, FieldScodec}
import com.odenzo.ripple.bincodec.scodecs.FieldIdScodec.{FieldCode, TypeCode}
import scodec.bits.Bases.Alphabets.HexUppercase

object Setup {

  //<editor-fold desc="Data Modelling Zone that will Need to be Optimized">
  val config: RippleConfig = RippleConfig.loadFromDefaultFile().fold(e => throw e, identity)
  scribe.trace(s"Raw Data Types: ${pprint.apply(config.types)}")

  val dataTypes: Map[String, Int]                                                              = config.types.toList.filter(v => v._2 >= 0).toMap
  val fields: List[FieldEntry]                                                                 = config.fields.filter(v => (v.metadata.nth > 0) && (v.metadata.nth < 256))
  val fieldsByFieldTypId: List[(FieldEntry, (BitVector, (FieldCode, FieldCode), Int, String))] = bindFieldIdToFields()
  scribe.trace(s"Data Types: ${pprint.apply(dataTypes)}")

  val txntypeIMap: Map[Int, String]               = config.transactionTypes.toList.map(_.swap).toMap
  val ledgerEntrytypeIMap: Map[FieldCode, String] = config.ledgerEntryTypes.toList.map(_.swap).toMap

  val datatypeCode2datatypeNameMap: Map[Int, String] = dataTypes
    .toList
    .map(x => (x._2, x._1))
    .toMap

  def bindFieldIdToFields(): List[(FieldEntry, (BitVector, (FieldCode, Int), Int, String))] = {
    import cats._
    import cats.data._
    import cats.implicits._

    scribe.trace(s"binding fieldids to fields")

    fields.fproduct { fe =>
      scribe.trace(s"binding field ${pprint.apply(fe)}")
      val typename        = fe.metadata.typeName
      val typecode        = dataTypes(typename)
      val fieldcode       = fe.metadata.nth
      val typecodeAdj     = if (typecode > 1000) 16 else typecode
      val encodedFielduid = FieldIdScodec.xrpfieldid.encode((fieldcode, typecodeAdj)).require
      scribe.trace(s"FieldCode: $fieldcode  TypeCode: $typecode \t Encoded: ${encodedFielduid.toHex(HexUppercase)}")
      (encodedFielduid, (fieldcode, typecode), typecode, typename)
    }
  }

  def findFieldByFieldId(fieldtype: (FieldCode, TypeCode)) = {
    fieldsByFieldTypId
      .find(x => x._2._2 === fieldtype)
      .getOrElse(throw new Exception(s"$fieldtype not found in " + s"field list"))
  }

  def getTransactionType(code: Int): String     = txntypeIMap(code)
  def getTransactionTypeCode(name: String): Int = config.transactionTypes(name)

  def getLedgerEntryType(code: Int): String     = ledgerEntrytypeIMap(code)
  def getLedgerEntryTypeCode(name: String): Int = config.ledgerEntryTypes(name)

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

  def decode(hex: String): Attempt[DecodeResult[Vector[(Json, Json)]]] = {
    val binary: BitVector = BitVector.fromHex(hex).getOrElse(throw new Exception("Invalid Input Hex"))
    decode(binary)
  }

  def decode(bv: BitVector): Attempt[DecodeResult[Vector[(Json, Json)]]] = {
    scribe.debug(s"Decoding Top Vector of Fields from ${bv.size}")
    scodec.codecs.vector(FieldScodec.xrpfield).decode(bv)
  }
}
