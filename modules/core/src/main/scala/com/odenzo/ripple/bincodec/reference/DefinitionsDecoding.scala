package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import io.circe.generic.extras.Configuration
import io.circe.{Codec, Json, Decoder}
import io.circe.generic.extras.semiauto._

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}

object DefinitionsDecoding extends JsonUtils {

  case class DefinitionJson(types: Map[String, Long],
                            ledger_entry_types: Map[String, Long],
                            transaction_types: Map[String, Long],
                            transaction_results: Map[String, Long],
                            fields: List[(String, FieldType)])

  object DefinitionJson {
    implicit val config: Configuration                 = Configuration.default.copy(transformMemberNames = (s: String) => s.toUpperCase)
    implicit val codec: Codec.AsObject[DefinitionJson] = deriveConfiguredCodec[DefinitionJson]
  }

  /** This converts to canonical FieldMetaData map by denormalizing the type field with the actual
    * RippleDataType. Error if FieldDefinition type not found in RippleDataType map. Corrupt file. */
  def convertToFieldMetaData(m: List[(String, FieldType)],
                             dt: Map[String, RippleDataType]): Either[OErrorRipple, Map[String, FieldMetaData]] = {

    val kv: Either[OErrorRipple, List[(String, FieldMetaData)]] = m.traverse {
      case (name, fd) =>
        val fmd: Either[OErrorRipple, FieldMetaData] = for {
          rdt <- dt.get(fd.tipe).toRight(RippleCodecError(s"${fd.tipe} from field $name not found"))
        } yield FieldMetaData(name, fd.nth, fd.isVLEncoded, fd.isSerialized, fd.isSigningField, rdt)
        fmd.tupleLeft(name)
    }
    kv.map(_.toMap)
  }

  def decodeDefinitions(json: Json): Either[RippleCodecError, DefinitionData] = {
    for {
      decoded        <- JsonUtils.decode(json, Decoder[DefinitionJson])
      dataTypes      = decoded.types.transform { case (name, num) => RippleDataType(name, num) }
      filteredFields = decoded.fields.filter { case (k, v) => v.isSerialized || v.isSigningField }
      fields         <- convertToFieldMetaData(filteredFields, dataTypes)
    } yield
      DefinitionData(fields,
                     decoded.types,
                     decoded.ledger_entry_types, // Careful with order as not well typed
                     decoded.transaction_types,
                     decoded.transaction_results)
  }

}
