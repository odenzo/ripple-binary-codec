package com.odenzo.ripple.bincodec.utils

import java.io.File

import cats.Monoid
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.Decoder.Result
import io.circe.jawn.JawnParser
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Json, JsonObject, Printer}

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{AppJsonDecodingError, AppJsonParsingError, BinCodecExeption, OErrorRipple, RippleCodecError}

private[bincodec] trait JsonUtils extends StrictLogging {


  /** Monoid/Semigroup for Circe Json Object so we can add them togeher. */
  implicit val jsonObjectMonoid: Monoid[JsonObject] = new Monoid[JsonObject] {
    def empty: JsonObject = JsonObject.empty

    def combine(x: JsonObject, y: JsonObject): JsonObject = JsonObject.fromIterable(x.toVector |+| y.toVector)
  }

  

  def findField(name: String, json: JsonObject): Either[RippleCodecError, Json] = {
    Either.fromOption(json(name), RippleCodecError(s"Field $name not found ", json.asJson))
  }


  def findFieldAsObject(name: String, json: JsonObject): Either[RippleCodecError, JsonObject] = {
    findField(name,json).flatMap(json2object)
  }

  
  def json2object(json: Json): Either[RippleCodecError, JsonObject] = {
    Either.fromOption(json.asObject, RippleCodecError("Expected JSON Object", json))
  }

  def json2array(json: Json): Either[RippleCodecError, List[Json]] = {
    Either.fromOption(json.asArray.map(_.toList), RippleCodecError("Expected JSON Array", json))
  }

  def json2string(json: Json): Either[RippleCodecError, String] = {
    Either.fromOption(json.asString, RippleCodecError("Expected JSON String", json))
  }

  def extractFieldFromObject(jobj: JsonObject, fieldName: String): Either[OErrorRipple, Json] = {
    Either.fromOption(jobj.apply(fieldName), RippleCodecError(s"Could not Find $fieldName in JSonObject "))
  }

  /**
    * Little utility for common case where an JsonObject just has "key": value
    * WHere value may be heterogenous?
    *
    * @param json
    */
  def extractAsKeyValueList(json: Json): ErrorOr[List[(String, Json)]] = {
    val obj: Either[OErrorRipple, JsonObject] = json.asObject.toRight(RippleCodecError("JSON Fragment was not a JSON Object"))
    val ans: Either[OErrorRipple, List[(String, Json)]] = obj.map(_.toList)
    ans
  }

  /**
    * Parses the list of json key  value pairs until it hits first error (not-accumulating parsing).
    *
    * @param json
    * @param fn
    * @tparam T
    *
    * @return
    */
  def parseKeyValuesList[T](json: Json, fn: (String, Json) ⇒ Either[RippleCodecError, T]): ErrorOr[List[T]] = {
    val kvs: ErrorOr[List[(String, Json)]] = extractAsKeyValueList(json)
    kvs.flatMap{ theList ⇒
      theList.traverse((tup: (String, Json)) ⇒ fn(tup._1, tup._2))
    }
  }


  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

  /** Converts json to formatted text dropping null JsonObject fields.
    *
    * @param json
    *
    * @return
    */
  def print(json: Json): String = json.pretty(droppingNullsPrinter)

  def print(jsonObject: JsonObject): String = print(jsonObject.asJson)

  /** For now does top level pruning of null fields from JSON Object
    * Now recurses */
  def pruneNullFields(obj: JsonObject): JsonObject = {
    obj
    .filter{
      case (field, value) ⇒ !value.isNull
    }
    .mapValues{ js: Json ⇒
      js.asObject match {
        case Some(obj) ⇒ pruneNullFields(obj).asJson
        case None      ⇒ js
      }
    }
    .asJsonObject

  }

  /** Caution: Uses BigDecimal and BigInt in parsing.
    *
    * @param m The text, in this case the response message text from websocket.
    *
    * @return JSON or an exception if problems parsing, error holds the original String.
    */
  def parseAsJson(m: String): ErrorOr[Json] = {
    io.circe.parser.parse(m).leftMap{ pf ⇒
      new AppJsonParsingError("Error Parsing String to Json", m, pf)
    }
  }

  def parseAsJsonObject(m: String): ErrorOr[JsonObject] = {
    parseAsJson(m).flatMap(json2jsonObject)
  }

  def json2jsonObject(json: Json): ErrorOr[JsonObject] = {
    Either.fromOption(json.asObject, RippleCodecError("JSON was not a JSonObject"))
  }

  def parseAsJson(f: File): ErrorOr[Json] = {
    logger.info(s"Parsing FIle $f")
    new JawnParser().parseFile(f).leftMap{ pf ⇒
      new BinCodecExeption(s"Error Parsing File $f to Json", pf)
    }
  }

  /**
    * {{{
    *    CirceUtils.decode(json.as[List[Foo]], json, "Decoding all Foo in the Bar")
    * }}}
    *
    * @param v
    * @param json
    * @param note
    * @tparam T
    *
    * @return
    */
  def decode[T](v: Result[T], json: Json, note: String = "No Clues"): ErrorOr[T] = {
    v.leftMap { err: DecodingFailure ⇒
      new AppJsonDecodingError(json, err, note)
    }
  }

  def decode[T](json: Json, decoder: Decoder[T]): ErrorOr[T] = {
    //val targs = typeOf[T] match { case TypeRef(_, _, args) => args }
    //val tmsg = s"type of $decoder has type arguments $targs"

    val decoderInfo = decoder.toString
    val msg         = s"Using Decoder $decoderInfo for Type"
    decoder.decodeJson(json).leftMap((e: DecodingFailure) ⇒ new AppJsonDecodingError(json, e, msg))
  }
}

private[bincodec] object JsonUtils extends JsonUtils
