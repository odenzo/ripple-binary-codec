package com.odenzo.ripple.bincodec.utils

import cats.Monoid
import cats.implicits._
import io.circe.optics.JsonPath
import io.circe.syntax._
import io.circe.{Json, Printer, JsonObject, Decoder}

import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr

private[bincodec] trait JsonUtils {

  /** Monoid/Semigroup for Circe Json Object so we can add them togeher combining top level fields only */
  implicit val jsonObjectMonoid: Monoid[JsonObject] = new Monoid[JsonObject] {
    def empty: JsonObject                                 = JsonObject.empty
    def combine(x: JsonObject, y: JsonObject): JsonObject = JsonObject.fromIterable(x.toVector |+| y.toVector)
  }

  def findField(name: String, json: Json): Either[BinCodecLibError, Json] = {
    JsonPath.root.at(name).getOption(json).flatten.toRight(BinCodecLibError(s"Field $name not found ", json.asJson))
  }

  def json2object(json: Json): Either[BinCodecLibError, JsonObject] = {
    json.asObject.toRight(BinCodecLibError("Expected JSON Object", json))
  }

  def json2array(json: Json): Either[BinCodecLibError, List[Json]] = {
    json.asArray.map(_.toList).toRight(BinCodecLibError("Expected JSON Array", json))
  }

  def json2bigint(json: Json): Either[BinCodecLibError, BigInt] = {
    json.asNumber
      .toRight(BinCodecLibError("Expected JSON Number", json))
      .flatMap(_.toBigInt.toRight(BinCodecLibError("JSON Number Not a Valid BigInt")))
  }

  def json2long(json: Json): Either[BinCodecLibError, Long] = {
    json.asNumber
      .toRight(BinCodecLibError("Expected JSON Number", json))
      .flatMap(_.toLong.toRight(BinCodecLibError("JSON Number Not a Valid Long")))
  }

  def json2string(json: Json): Either[BinCodecLibError, String] = {
    json.asString.toRight(BinCodecLibError("Expected JSON String", json))
  }

  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

  def removeAllNulls(j: JsonObject): ErrorOr[JsonObject] = {
    val cleanTxt = droppingNullsPrinter.print(j.asJson)
    parseAsJson(cleanTxt).flatMap(json2object)
  }

  /** Caution: Uses BigDecimal and BigInt in parsing.
    *
    * @param m The text, in this case the response message text from websocket.
    *
    * @return JSON or an exception if problems parsing, error holds the original String.
    */
  def parseAsJson(m: String): ErrorOr[Json] = {
    io.circe.parser.parse(m).leftMap(pf => BCJsonParseErr("Parsing String to Json", m, pf))
  }

  def decode[T](json: Json, decoder: Decoder[T], msg: Option[String] = None): ErrorOr[T] = {
    decoder.decodeJson(json).leftMap(e => BCJsonDecodingErr(msg.getOrElse("Decoding Helper"), json, e))
  }
}

private[bincodec] object JsonUtils extends JsonUtils
