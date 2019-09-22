package com.odenzo.ripple.bincodec.utils

import cats.Monoid
import cats.implicits._
import io.circe.optics.JsonPath
import io.circe.syntax._
import io.circe.{Json, Printer, JsonObject, Decoder}

import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr

private[bincodec] trait JsonUtils {

  /** Monoid/Semigroup for Circe Json Object so we can add them togeher. */
  implicit val jsonObjectMonoid: Monoid[JsonObject] = new Monoid[JsonObject] {
    def empty: JsonObject = JsonObject.empty

    def combine(x: JsonObject, y: JsonObject): JsonObject = JsonObject.fromIterable(x.toVector |+| y.toVector)
  }

  def findField(name: String, json: Json): Either[BinCodecLibError, Json] = {
    JsonPath.root.at(name).getOption(json).flatten.toRight(BinCodecLibError(s"Field $name not found ", json.asJson))
  }

  /**
    * Finds the first (nested) field of given name which is expected to be a JsonObject.
    *
    * @param field Field name, deep search done.
    * @param obj   Json to search on, this is typically but no always a JsonObject
    *
    * @return Error if field not found, or *first* found field is not JsonObject, Success is the JsonObject.
    */
  def findFieldDeepAsObject(field: String, obj: Json): Either[BinCodecLibError, JsonObject] = {
    obj
      .findAllByKey(field)
      .collectFirst {
        case obj if obj.isObject => json2object(obj)
        case other               => BinCodecLibError(s"First $field field was not JsonObject").asLeft
      }
      .getOrElse {
        BinCodecLibError(s"FieldName $field was not found").asLeft
      }
  }

  def json2object(json: Json): Either[BinCodecLibError, JsonObject] = {
    json.asObject.toRight(BinCodecLibError("Expected JSON Object", json))
  }

  def json2array(json: Json): Either[BinCodecLibError, List[Json]] = {
    json.asArray.map(_.toList).toRight(BinCodecLibError("Expected JSON Array", json))
  }

  def json2string(json: Json): Either[BinCodecLibError, String] = {
    json.asString.toRight(BinCodecLibError("Expected JSON String", json))
  }

  /**
    * Little utility for common case where an JsonObject just has "key": value
    * WHere value may be heterogenous?
    *
    * @param json
    */
  def extractAsKeyValueMap(json: Json): Either[BinCodecLibError, Map[String, Json]] = {
    json2object(json).map(_.toMap)

  }

  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

  def removeAllNulls(j: JsonObject): ErrorOr[JsonObject] = {
    val cleanTxt = droppingNullsPrinter.print(j.asJson)
    parseAsJsonObject(cleanTxt)
  }

  /** Caution: Uses BigDecimal and BigInt in parsing.
    *
    * @param m The text, in this case the response message text from websocket.
    *
    * @return JSON or an exception if problems parsing, error holds the original String.
    */
  def parseAsJson(m: String): ErrorOr[Json] = {
    io.circe.parser.parse(m).leftMap { pf =>
      new BCJsonParseErr("Error Parsing String to Json", m, pf)
    }
  }

  def parseAsJsonObject(m: String): ErrorOr[JsonObject] = {
    parseAsJson(m).flatMap(json2object)
  }

  def decode[T](json: Json, decoder: Decoder[T], msg: Option[String] = None): ErrorOr[T] = {
    decoder
      .decodeJson(json)
      .leftMap(e => BCJsonDecodingErr(msg.getOrElse("Decoding Helper"), json, e))
  }
}

private[bincodec] object JsonUtils extends JsonUtils
