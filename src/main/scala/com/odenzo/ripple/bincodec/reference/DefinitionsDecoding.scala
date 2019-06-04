package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.Decoder.Result
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{AppJsonDecodingError, CodecError, OError}
import com.odenzo.ripple.models.utils.CirceCodecUtils

object DefinitionsDecoding extends StrictLogging with JsonUtils {

  implicit val rtypeDecoder: Decoder[RippleType] = deriveDecoder[RippleType]

  /** Main Decoder of the JSON file */
  def decodeDefinitionFileJson(json: Json): Either[CodecError, DefinitionData] = {
    val topLevelTpeKeys = List("TYPES", "LEDGER_ENTRY_TYPES", "TRANSACTION_TYPES", "TRANSACTION_RESULTS")

    // Top Level Object, with Keys mapping to other objects.
    val topJson: Either[CodecError, JsonObject] = json2object(json)

    val typesMap: Either[CodecError, Map[String, Map[String, Long]]] = topLevelTpeKeys
                                                                       .traverse { typeKey ⇒
        val tpeData: Either[CodecError, Map[String, Long]]       = topJson.flatMap(v ⇒ decodeTypes(v, typeKey))
        val res: Either[CodecError, (String, Map[String, Long])] = tpeData.map(v ⇒ (typeKey, v))
        res
      }
                                                                       .map(_.toMap)

    val fieldData: Either[CodecError, Map[String, FieldType]] = topJson
                                                                .flatMap(CirceCodecUtils.extractFieldFromObject(_, "FIELDS"))
                                                                .flatMap(decodeFields)

    val normalized = for {
      allTypes ← typesMap
      types    ← getMapEntry(allTypes, "TYPES")
      fields   ← fieldData
      info     ← mergeToInfo(fields, types)
      ledger   ← getMapEntry(allTypes, "LEDGER_ENTRY_TYPES")
      txn      ← getMapEntry(allTypes, "TRANSACTION_TYPES")
      txnres   ← getMapEntry(allTypes, "TRANSACTION_RESULTS")

    } yield DefinitionData(info, fields, types, ledger, txn, txnres)

    normalized
  }

  private def getMapEntry[T, V](map: Map[T, V], key: T): Either[OError, V] = {
    Either.fromOption(map.get(key), CodecError(s" $key not found in map"))
  }

  def rTypeFromKeyValue(key: String, json: Json): Either[CodecError, (String, Long)] = {
    AppJsonDecodingError
      .wrapResult(json.as[Long], json, s"Getting Key $key value")
      .map(l ⇒ (key, l))
  }

  /** ALl the different types have the same format. field: signed int  */
  def decodeTypes(json: JsonObject, name: String): ErrorOr[Map[String, Long]] = {
    CirceCodecUtils
      .extractFieldFromObject(json, name)
      .flatMap(v ⇒ CirceCodecUtils.parseKeyValuesList(v, rTypeFromKeyValue))
      .map(l ⇒ l.toMap)
  }

  /**
    * The fields section is a bit different, we decode, then we will merge with each field type
    * for efficiency
    *
    * @param fieldsRaw
    *
    * @return
    */
  def decodeFields(fieldsRaw: Json): ErrorOr[Map[String, FieldType]] = {
    // Seems like an array of arrays. The bottom array has  ["fieldName" , { obj }]  where obj matches fieldType

    val fields: Either[CodecError, List[Json]]             = json2array(fieldsRaw)
    val all: Either[CodecError, List[(String, FieldType)]] = fields.flatMap(ljson ⇒ ljson.traverse(decodeEachField))
    val ans: ErrorOr[Map[String, FieldType]]             = all.map(v ⇒ v.toMap)
    ans
  }

  /** Each field in the Field Type array */
  def decodeEachField(field: Json): Either[CodecError, (String, FieldType)] = {
    val arr: Either[CodecError, List[Json]] = json2array(field)
    arr.flatMap { vec: List[Json] ⇒ // This is array with head the field name and second the field data
      vec.toList match {
        case fieldName :: obj :: Nil =>
          val name: Result[String]  = fieldName.as[String]
          val ft: Result[FieldType] = obj.as[FieldType]
          AppJsonDecodingError.wrapResult(name.product(ft), obj, "Definitions Bottom Field")

        case other ⇒ CodecError("Bottom Field Array Length != 2").asLeft // Should never happen
      }
    }
  }

  protected def mergeToInfo(fields: Map[String, FieldType],
                            types: Map[String, Long]): Either[OError, Map[String, FieldInfo]] = {
    val tuples = fields.toList
      .traverse {
        case (name: String, ft: FieldType) =>
          types
            .get(ft.tipe)
            .map( RippleType(ft.tipe,_))
            .map(rt ⇒ (name, FieldInfo(ft.nth, ft.isVLEncoded, ft.isSerialized, ft.isSigningField, rt)))
      }
      .map(_.toMap)

    Either.fromOption(tuples, CodecError("Couldnt Merge all the types"))
  }


}
