package com.odenzo.ripple.bincodec.setup

import cats._
import cats.data._
import cats.implicits._
import io.circe.generic.extras.Configuration
import io.circe.{Codec, Decoder, Json}
import io.circe.generic.extras.semiauto._

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.{BCLibErr, BinCodecLibError}
//
//object DefinitionsDecoding extends JsonUtils {
//
//  case class DefinitionJson(
//      types: Map[String, Long],
//      ledger_entry_types: Map[String, Long],
//      transaction_types: Map[String, Long],
//      transaction_results: Map[String, Long],
//      fields: List[(String, FieldType)]
//  )
//
//  object DefinitionJson {
//    implicit val config: Configuration                 = Configuration.default.copy(transformMemberNames = (s: String) => s.toUpperCase)
//    implicit val codec: Codec.AsObject[DefinitionJson] = deriveConfiguredCodec[DefinitionJson]
//  }
//
//  /** This converts to canonical FieldMetaData map by denormalizing the type field with the actual
//    * RippleDataType. Error if FieldDefinition type not found in RippleDataType map. Corrupt file. */
//  def convertToFieldMetaData(m: List[(String, FieldType)], dt: Map[String, RippleDataType]): Either[BCLibErr, List[FieldMetaData]] = {
//    m.traverse {
//      case (name, fd) =>
//        for {
//          rdt <- dt.get(fd.tipe).toRight(BinCodecLibError(s"${fd.tipe} from field $name not found"))
//        } yield FieldMetaData(name, fd.nth, fd.isVLEncoded, fd.isSerialized, fd.isSigningField, rdt)
//    }
//  }
//
//  def decodeDefinitions(json: Json): Either[BinCodecLibError, DefinitionData] = {
//    import scodec.codecs._
//    for {
//      decoded <- JsonUtils.decode(json, Decoder[DefinitionJson])
//      // Well, this are UInt6 but some are negatice
//      dataTypes = decoded.types.transform {
//        case (name, num) =>
//          val preEncode = uint16
//            .encode(num.toInt)
//            .map(_.bytes)
//            .toEither
//            .leftMap(e => BinCodecLibError(e.messageWithContext))
//          RippleDataType(name, num, preEncode)
//      }
//      filteredFields = decoded.fields.filter { case (k, v) => v.isSerialized || v.isSigningField }
//      fields <- convertToFieldMetaData(filteredFields, dataTypes)
//      fieldsByName   = fields.map(field => field.name          -> field).toMap
//      fieldsByMarker = fields.map(field => field.fieldID.bytes -> field).toMap
//    } yield DefinitionData(
//      fieldsByName,
//      fieldsByMarker,
//      dataTypes,
//      decoded.ledger_entry_types.transform(toMnemonicType),
//      decoded.transaction_types.transform(toMnemonicType),
//      decoded.transaction_results.transform(toMnemonicType)
//    )
//
//  }
//
//  def toMnemonicType(name: String, num: Long): MnemonicType = {
//    import scodec.codecs.uint16
//    MnemonicType(name, num, uint16.encode(num.toInt).toEither.leftMap(err => BinCodecLibError(err.messageWithContext)).map(_.bytes))
//  }
//}

//object DefinitionData {
//
//  import scodec.bits._
//
//  val pathSetAnother  = hex"FF" // indicates another path follows
//  val pathSetEnd      = hex"00" // indicates the end of the PathSet
//  val objDel          = hex"0F" // Object Delimeter in some packed fields forget which
//  val objectEndMarker = hex"E1" // indicates end of object this is STObject not blob
//  val arrDel          = hex"0D" // Array delimeter
//  val arrayEndMarker  = hex"F1" // End of Array
//
//  final val objectMarkerEndName: String = "ObjectEndMarker"
//  final val arrayMarkerEndName : String = "ArrayEndMarker"
//
//  implicit val showFieldInfo: Show[FieldMetaData] = Show[FieldMetaData]{ fi =>
//    s"${fi.fieldID.toHex} : ${fi.name} ${fi.datatype.name} : VL ${fi.isVLEncoded} " +
//    s" Serialized/Signing ${fi.isSerialized}/${fi.isSigningField} "
//  }
//
//  implicit val show: Show[DefinitionData] = Show[DefinitionData]{ dd =>
//    val sortedFields: String =
//      dd.fieldByName
//        .values
//        .toList
//        .sortBy(e => (e.fieldID.toHex.length, e.fieldID.toHex))
//        .map(info => info.show)
//        .mkString("\n")
//
//    sortedFields
//
//  }
//}
