package com.odenzo.ripple.bincodec.setup

import io.circe.Json
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.BitVector
import spire.math._
import spire.implicits._

import com.odenzo.ripple.bincodec.config.{FieldEntry, RippleConfig}
import com.odenzo.ripple.bincodec.scodecs.{FieldIdScodec, TrivialScodec}
import com.odenzo.ripple.bincodec.scodecs.FieldIdScodec.{FieldCode, TypeCode}
import io.circe.syntax._
import scodec.bits.Bases.Alphabets.HexUppercase

object Setup {

  val config: RippleConfig = RippleConfig.loadFromDefaultFile().fold(e => throw e, identity)

  scribe.info(s"Raw Data Types: ${pprint.apply(config.types)}")

  val dataTypes: Map[String, Int] = config.types.toList.filter(v => v._2 >= 0).toMap
  val fields: List[FieldEntry]    = config.fields.filter(v => (v.metadata.nth > 0) && (v.metadata.nth < 256))

  scribe.info(s"Data Types: ${pprint.apply(dataTypes)}")

  val datatypeCode2datatypeNameMap: Map[Int, String] = dataTypes
    .toList
    .map(x => (x._2, x._1))
    .toMap

  /** What do I really want... since vals. I need a fieldMarker <-> scodec for sure */

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

  def datetypeCodeToName(datatypecode: Int): String = {
    datatypeCode2datatypeNameMap(datatypecode)
  }

  // Decoder thoughts:
  // Given full bitvector:
  // DecodeFieldId map DataTypeId match to decoder apply decoding return dependantType (Any) or decode to ADT model
  // or decode directly to JSON (?), that sounds good actually.

  //def bindScodecs(types: Map[String, Int], binding: Map[String, Codec[Nothing]]) = {}
}

object DecoderController {

  def decode(hex: String) = {
    val binary: BitVector = BitVector.fromHex(hex).getOrElse(throw new Exception("Invalid Input Hex"))

    //  scodec.codecs.vector(xrpfieldcodec)
  }

  // Need to checkout the other branch, the top level txn thing doesn't have a field, just a STObject with no end marker.
  // So, the top driver is simply  decoderNextField  while bitvector.is not empty

  /** Start of xrpfieldcodec. The decoding part. The encoder will have to have JSON with current approach
    *  I want to try and see if I can write it to easily replace the model with existing case classes later.
    *
   **/
  def decodeNextField(bv: BitVector): DecodeResult[(Json, Json)] = {
    val idMaybe: DecodeResult[(FieldCode, TypeCode)] = FieldIdScodec.xrpfieldid.decode(bv).require
    val (fc, tc: TypeCode)                           = idMaybe.value
    val field                                        = Setup.config.fields.find(_.metadata.nth == fc.toInt).getOrElse(throw new Exception(s"Missing Field $fc"))
    val typeName                                     = field.metadata.typeName
    val result: DecodeResult[Json]                   = ScodecDataTypeBinding.dynamicDecode(bv, typeName).require
    result.map(fieldVal => field.name.asJson -> fieldVal)
  }
}
