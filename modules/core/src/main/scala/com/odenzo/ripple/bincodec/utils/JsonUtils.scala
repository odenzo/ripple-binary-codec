package com.odenzo.ripple.bincodec.utils

import java.io.File

import cats.Monoid
import cats.implicits._
import io.circe.jawn.JawnParser
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

  def findField(name: String, json: JsonObject): Either[BinCodecLibError, Json] = {
    Either.fromOption(json(name), BinCodecLibError(s"Field $name not found ", json.asJson))
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
    Either.fromOption(json.asObject, BinCodecLibError("Expected JSON Object", json))
  }

  def json2array(json: Json): Either[BinCodecLibError, List[Json]] = {
    Either.fromOption(json.asArray.map(_.toList), BinCodecLibError("Expected JSON Array", json))
  }

  def json2string(json: Json): Either[BinCodecLibError, String] = {
    Either.fromOption(json.asString, BinCodecLibError("Expected JSON String", json))
  }

  def extractFieldFromObject(jobj: JsonObject, fieldName: String): Either[BCLibErr, Json] = {
    Either.fromOption(jobj.apply(fieldName), BinCodecLibError(s"Could not Find $fieldName in JSonObject "))
  }

  /**
    * Little utility for common case where an JsonObject just has "key": value
    * WHere value may be heterogenous?
    *
    * @param json
    */
  def extractAsKeyValueList(json: Json): ErrorOr[List[(String, Json)]] = {
    val obj: Either[BCLibErr, JsonObject] =
      json.asObject.toRight(BinCodecLibError("JSON Fragment was not a JSON Object"))
    val ans: Either[BCLibErr, List[(String, Json)]] = obj.map(_.toList)
    ans
  }

  /**
    * Parses the list of json key  value pairs until it hits first error (not-accumulating parsing).
    *
    * @param json
    * @param fn The parsing function
    * @tparam T
    *
    * @return
    */
  def parseKeyValuesList[T](json: Json, fn: (String, Json) => Either[BinCodecLibError, T]): ErrorOr[List[T]] = {
    val kvs: ErrorOr[List[(String, Json)]] = extractAsKeyValueList(json)
    kvs.flatMap { theList =>
      theList.traverse { case (key: String, value: Json) => fn(key, value) }
    }

  }

  def removeAllNulls(j: JsonObject): ErrorOr[JsonObject] = {
    val cleanTxt = droppingNullsPrinter.print(j.asJson)
    parseAsJsonObject(cleanTxt)
  }

  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

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
    parseAsJson(m).flatMap(json2jsonObject)
  }

  def json2jsonObject(json: Json): ErrorOr[JsonObject] = {
    Either.fromOption(json.asObject, BinCodecLibError("JSON was not a JSonObject"))
  }

  def parseAsJson(f: File): ErrorOr[Json] = {
    scribe.info(s"Parsing FIle $f")
    new JawnParser().parseFile(f).leftMap { pf =>
      new BCException(s"Error Parsing File $f to Json", pf)
    }
  }

  def decode[T](json: Json, decoder: Decoder[T], msg: Option[String] = None): ErrorOr[T] = {
    decoder
      .decodeJson(json)
      .leftMap(e => BCJsonDecodingErr(msg.getOrElse("Decoding Helper"), json, e))
  }
}

private[bincodec] object JsonUtils extends JsonUtils
