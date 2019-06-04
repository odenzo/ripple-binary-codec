package com.odenzo.ripple.bincodec.utils

import java.io.File

import cats._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.Decoder.Result
import io.circe._
import io.circe.jawn.JawnParser
import io.circe.syntax._

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{AppException, AppJsonDecodingError, AppJsonParsingError, CodecError}

/**
  *  Traits for working with Circe DOM [[io.circe.Json]]
  */
trait CirceUtils extends StrictLogging {

  /** Ripled doesn't like objects like { x=null } */
  val droppingNullsPrinter: Printer = Printer.spaces2.copy(dropNullValues = true)

  /** Converts json to formatted text dropping null JsonObject fields.
    *  @param json
    *  @return
    */
  def print(json: Json): String = json.pretty(droppingNullsPrinter)

  def print(jsonObject: JsonObject): String = print(jsonObject.asJson)

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
    *  @param m The text, in this case the response message text from websocket.
    *
    *  @return JSON or an exception if problems parsing, error holds the original String.
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
    Either.fromOption(json.asObject, CodecError("JSON was not a JSonObject"))
  }

  def parseAsJson(f: File): ErrorOr[Json] = {
    logger.info(s"Parsing FIle $f")
    new JawnParser().parseFile(f).leftMap { pf ⇒
      new AppException(s"Error Parsing File $f to Json", pf)
    }
  }

  /** Monoid/Semigroup for Circe Json Object so we can add them togeher. */
  implicit val jsonObjectMonoid: Monoid[JsonObject] = new Monoid[JsonObject] {
    def empty: JsonObject                                 = JsonObject.empty
    def combine(x: JsonObject, y: JsonObject): JsonObject = JsonObject.fromIterable(x.toVector |+| y.toVector)
  }

  /**
    *  {{{
    *    CirceUtils.decode(json.as[List[Foo]], json, "Decoding all Foo in the Bar")
    *  }}}
    * @param v
    * @param json
    * @param note
    * @tparam T
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

object CirceUtils extends CirceUtils
