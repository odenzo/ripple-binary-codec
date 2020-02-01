package com.odenzo.ripple.bincodec.config

import scala.io.Source
import scala.util.Try

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec

case class FieldEntry(name: String, metadata: FieldMeta)

object FieldEntry {

  // Enter with c at the Array with two elements, 0=String, 1 = JsonObject
  implicit val decoder: Decoder[FieldEntry] = new Decoder[FieldEntry] {

    final def apply(c: HCursor): Decoder.Result[FieldEntry] =
      for {
        name <- c.downArray.as[String]
        tipe <- c.downN(1).as[FieldMeta]
      } yield {
        new FieldEntry(name, tipe)
      }
  }

  implicit val encoder: Encoder[FieldEntry] = new Encoder[FieldEntry] {
    import io.circe.syntax._
    override def apply(a: FieldEntry) = List(a.name.asJson, a.metadata.asJson).asJson
  }

}

/** Note that tipe is a String matching kv  (Int maybe Short)*/
case class FieldMeta(nth: Int, isVLEncoded: Boolean, isSerialized: Boolean, isSigningField: Boolean, typeName: String)

object FieldMeta {

  implicit val config: Configuration =
    Configuration.default.copy(transformMemberNames = (s: String) => if (s === "typeName") "type" else s)
  implicit val codec: Codec.AsObject[FieldMeta] = deriveConfiguredCodec[FieldMeta]
}

case class RippleConfig(
    types: Map[String, Int],
    fields: List[FieldEntry],
    ledgerEntryTypes: Map[String, Int],
    transactionResults: Map[String, Int],
    transactionTypes: Map[String, Int]
)

object RippleConfig {

  def loadFromDefaultFile(): Either[Throwable, RippleConfig] = {
    val resourcePath = "definitions.json"
    scribe.warn(s"Loading from Path: $resourcePath")
    Try {
      Source.fromResource(resourcePath)
    }.adaptErr {
        case e: Exception => new IllegalStateException(s"Failed Loading Resource: $resourcePath")
      }
      .toEither
      .flatMap { src =>
        scribe.info(s"Got the Src from $resourcePath => $src")
        val t = src.mkString
        scribe.trace(s"Configuration Text:\n$t")
        loadFromText(t)
      }
  }

  def loadFromText(data: String): Either[Error, RippleConfig] = {
    io.circe.parser.decode[RippleConfig](data)
  }

  implicit val config: Configuration =
    Configuration.default.copy(transformMemberNames = Configuration.snakeCaseTransformation(_).toUpperCase)
  implicit val codec: Codec.AsObject[RippleConfig] = deriveConfiguredCodec[RippleConfig]
}
