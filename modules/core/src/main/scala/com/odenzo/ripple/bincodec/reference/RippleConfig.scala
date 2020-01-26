package com.odenzo.ripple.bincodec.reference

import scala.io.Source
import scala.util.{Failure, Success, Try}

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec

case class FieldEntry(name: String, tipe: FieldType)

object FieldEntry {

  // Enter with c at the Array with two elements, 0=String, 1 = JsonObject
  implicit val decoder: Decoder[FieldEntry] = new Decoder[FieldEntry] {

    final def apply(c: HCursor): Decoder.Result[FieldEntry] =
      for {
        name <- c.downArray.as[String]
        tipe <- c.downN(1).as[FieldType]
      } yield {
        new FieldEntry(name, tipe)
      }
  }

  implicit val encoder: Encoder[FieldEntry] = new Encoder[FieldEntry] {
    import io.circe.syntax._
    override def apply(a: FieldEntry) = List(a.name.asJson, a.tipe.asJson).asJson
  }

}

/** Note that tipe is a String matching kv  (Int maybe Short)*/
case class FieldType(nth: Int, isVLEncoded: Boolean, isSerialized: Boolean, isSigningField: Boolean, tipe: String)

object FieldType {

  implicit val config: Configuration =
    Configuration.default.copy(transformMemberNames = (s: String) => if (s === "tipe") "type" else s)
  implicit val codec: Codec.AsObject[FieldType] = deriveConfiguredCodec[FieldType]
}

case class RippleConfig(
    types: Map[String, Int],
    fields: List[FieldEntry],
    ledgerEntryTypes: Map[String, Int],
    transactionResults: Map[String, Int],
    transactionTypes: Map[String, Int]
)

object RippleConfig {

  def loadFromDefaultFile() = {
    val resourcePath = "definitions.json"
    scribe.warn(s"Loading from Path: $resourcePath")
    Try {
      Source.fromResource(resourcePath)
    } match {
      case Failure(e) =>
        scribe.error(s"Failed Loading $resourcePath with", e)
        throw new IllegalStateException(s"Failed Loading Resource: $resourcePath")
      case Success(src) =>
        scribe.info(s"Got the Src from $resourcePath => $src")
        val t = src.mkString
        scribe.trace(s"Configuration Text:\n$t")
        loadFromText(t)
    }
  }

  def loadFromText(data: String) = {
    io.circe.parser.decode[RippleConfig](data)
  }

  implicit val config: Configuration =
    Configuration.default.copy(transformMemberNames = Configuration.snakeCaseTransformation(_).toUpperCase)
  implicit val codec: Codec.AsObject[RippleConfig] = deriveConfiguredCodec[RippleConfig]
}
