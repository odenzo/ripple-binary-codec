package com.odenzo.ripple.bincodec.utils

import java.io.File

import cats.Monoid
import cats.implicits._
import io.circe.jawn.JawnParser
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Json, JsonObject, Printer}

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{
  AppJsonDecodingError,
  AppJsonParsingError,
  BinCodecExeption,
  OErrorRipple,
  RippleCodecError
}

private[bincodec] trait JsonUtils {

  /** Monoid/Semigroup for Circe Json Object so we can add them togeher. */
  implicit val jsonObjectMonoid: Monoid[JsonObject] = new Monoid[JsonObject] {
    def empty: JsonObject = JsonObject.empty

    def combine(x: JsonObject, y: JsonObject): JsonObject = JsonObject.fromIterable(x.toVector |+| y.toVector)
  }

  def findField(name: String, json: JsonObject): Either[RippleCodecError, Json] = {
    Either.fromOption(json(name), RippleCodecError(s"Field $name not found ", json.asJson))
  }

  /** Finds top level field */
  def findFieldAsObject(name: String, json: JsonObject): Either[RippleCodecError, JsonObject] = {
    findField(name, json).flatMap(json2object)
  }


  /**
    * Finds the first (nested) field of given name which is expected to be a JsonObject.
    *
    * @param field Field name, deep search done.
    * @param obj   Json to search on, this is typically but no always a JsonObject
    *
    * @return Error if field not found, or *first* found field is not JsonObject, Success is the JsonObject.
    */
  def findFieldDeepAsObject(field: String, obj: Json): Either[RippleCodecError, JsonObject] = {
    obj.findAllByKey(field).collectFirst{
      case obj if obj.isObject ⇒ json2object(obj)
      case other               ⇒ RippleCodecError(s"First $field field was not JsonObject").asLeft
    }.getOrElse{
      RippleCodecError(s"FieldName $field was not found").asLeft
    }
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
    val obj: Either[OErrorRipple, JsonObject] =
      json.asObject.toRight(RippleCodecError("JSON Fragment was not a JSON Object"))
    val ans: Either[OErrorRipple, List[(String, Json)]] = obj.map(_.toList)
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
  def parseKeyValuesList[T](json: Json, fn: (String, Json) ⇒ Either[RippleCodecError, T]): ErrorOr[List[T]] = {
    val kvs: ErrorOr[List[(String, Json)]] = extractAsKeyValueList(json)
    kvs.flatMap { theList ⇒
      theList.traverse { case (key: String, value: Json) ⇒ fn(key, value) }
    }
    
  }

  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

  /** For now does top level pruning of null fields from JSON Object
    * Now recurses */
  def pruneNullFields(obj: JsonObject): JsonObject = {
    obj
      .filter {
        case (field, value) ⇒ !value.isNull
      }
      .mapValues { js: Json ⇒
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
    io.circe.parser.parse(m).leftMap { pf ⇒
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
    scribe.info(s"Parsing FIle $f")
    new JawnParser().parseFile(f).leftMap { pf ⇒
      new BinCodecExeption(s"Error Parsing File $f to Json", pf)
    }
  }

  def decode[T](json: Json, decoder: Decoder[T], msg: Option[String] = None): ErrorOr[T] = {
    //val targs = typeOf[T] match { case TypeRef(_, _, args) => args }
    //val tmsg = s"type of $decoder has type arguments $targs"

    val decoderInfo = decoder.toString
    val errmsg      = msg.getOrElse(s"Using Decoder $decoderInfo for Type")
    decoder.decodeJson(json).leftMap((e: DecodingFailure) ⇒ new AppJsonDecodingError(json, e, errmsg))
  }
}

private[bincodec] object JsonUtils extends JsonUtils
