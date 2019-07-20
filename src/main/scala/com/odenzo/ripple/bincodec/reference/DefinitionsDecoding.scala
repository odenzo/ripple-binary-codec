package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import io.circe.Decoder.Result
import io.circe.generic.semiauto._
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{AppJsonDecodingError, OErrorRipple, RippleCodecError}

object DefinitionsDecoding extends JsonUtils {

  implicit val rtypeDecoder: Decoder[RippleDataType] = deriveDecoder[RippleDataType]

  /** Main Decoder of the JSON file. Parses all type and then de-normalizes
    * the field info and field types for speed. Also filters 'info' FieldInfo
    * to just contain fields that are serialized or signing fields.
    * (They remain in the original fields data.
    **/
  def decodeDefinitionFileJson(json: Json): Either[RippleCodecError, DefinitionData] = {
    val topLevelTpeKeys = List("TYPES", "LEDGER_ENTRY_TYPES", "TRANSACTION_TYPES", "TRANSACTION_RESULTS")

    // Top Level Object, with Keys mapping to other objects.
    val topJson: Either[RippleCodecError, JsonObject] = json2object(json)

    val typesMap: Either[RippleCodecError, Map[String, Map[String, Long]]] = topLevelTpeKeys
      .traverse { typeKey ⇒
        val tpeData: Either[RippleCodecError, Map[String, Long]] = topJson.flatMap(v ⇒ decodeDataTypes(v, typeKey))
        tpeData.map(v ⇒ (typeKey, v))
      }
      .map(_.toMap)

    val fieldData: Either[RippleCodecError, Map[String, FieldType]] = topJson
      .flatMap(extractFieldFromObject(_, "FIELDS"))
      .flatMap(decodeFields)

    val normalized = for {
      allTypes   ← typesMap
      types      ← getMapEntry(allTypes, "TYPES")
      fields     ← fieldData
      jsonFields = fields.filter(v ⇒ v._2.isSerialized || v._2.isSigningField)
      info       ← mergeFieldInfoWithDataType(jsonFields, types)
      ledger     ← getMapEntry(allTypes, "LEDGER_ENTRY_TYPES")
      txn        ← getMapEntry(allTypes, "TRANSACTION_TYPES")
      txnres     ← getMapEntry(allTypes, "TRANSACTION_RESULTS")

    } yield DefinitionData(info, fields, types, ledger, txn, txnres)

    normalized
  }

  protected def getMapEntry[T, V](map: Map[T, V], key: T): Either[OErrorRipple, V] = {
    Either.fromOption(map.get(key), RippleCodecError(s" $key not found in map"))
  }

  protected def rippleDataTypeFromKey(key: String, json: Json): Either[RippleCodecError, (String, Long)] = {
    AppJsonDecodingError
      .wrapResult(json.as[Long], json, s"Getting Key $key value")
      .map(l ⇒ (key, l))
  }

  /** ALl the different types have the same format. field: signed int  */
  protected def decodeDataTypes(json: JsonObject, name: String): ErrorOr[Map[String, Long]] = {
    JsonUtils
      .extractFieldFromObject(json, name)
      .flatMap(v ⇒ JsonUtils.parseKeyValuesList(v, rippleDataTypeFromKey))
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
    // An array of arrays. The bottom array has  ["fieldName" , { obj }]  where obj matches fieldType

    val fields: Either[RippleCodecError, List[Json]] = json2array(fieldsRaw)
    val all: Either[RippleCodecError, List[(String, FieldType)]] =
      fields.flatMap(ljson ⇒ ljson.traverse(decodeEachField))
    val ans: ErrorOr[Map[String, FieldType]] = all.map(v ⇒ v.toMap)
    ans
  }

  /** Each field in the Field Type array */
  def decodeEachField(field: Json): Either[RippleCodecError, (String, FieldType)] = {
    val arr: Either[RippleCodecError, List[Json]] = json2array(field)
    arr.flatMap { vec: List[Json] ⇒ // This is array with head the field name and second the field data
      vec match {
        case fieldName :: obj :: Nil =>
          val name: Result[String]  = fieldName.as[String]
          val ft: Result[FieldType] = obj.as[FieldType]
          AppJsonDecodingError.wrapResult(name.product(ft), obj, "Definitions Bottom Field")

        case other ⇒ RippleCodecError("Bottom Field Array Length != 2").asLeft // Should never happen
      }
    }
  }

  protected def mergeFieldInfoWithDataType(fields: Map[String, FieldType],
                                           types: Map[String, Long]): Either[OErrorRipple, Map[String, FieldMetaData]] = {
    val tuples = fields.toList
      .traverse {
        case (name: String, ft: FieldType) =>
          types
            .get(ft.tipe)
            .map(RippleDataType(ft.tipe, _))
            .map(rt ⇒ (name, FieldMetaData(name, ft.nth, ft.isVLEncoded, ft.isSerialized, ft.isSigningField, rt)))
      }
      .map(_.toMap)

    Either.fromOption(tuples, RippleCodecError("Couldnt Merge all the types"))
  }

}
