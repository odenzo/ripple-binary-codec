package com.odenzo.ripple.bincodec.reference

import scala.io.Source

import com.odenzo.ripple.bincodec.{OTestSpec, BinCodecLibError}
import io.circe._
import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.JsonUtils

class DefinitionDataTest extends OTestSpec {

  val resourceName = "/ripplereferencedata/definitions.json"
  scribe.info(s"Loading Default Data from $resourceName")

  val json: ErrorOr[Json] = Option(this.getClass.getResourceAsStream(resourceName)) match {
    case None => BinCodecLibError(s"Couldn't Find Definitions Resource $resourceName").asLeft
    case Some(s) =>
      val txt = Source.fromInputStream(s, "UTF-8").getLines().mkString("\n")
      JsonUtils.parseAsJson(txt)
  }

  test("Constants") {
    import scodec.bits.Bases.Alphabets.HexUppercase
    DefinitionData.pathSetEnd.toHex(HexUppercase) shouldEqual "00"
    DefinitionData.pathSetAnother.toHex shouldEqual "ff"
    DefinitionData.objectEndMarker.toHex shouldEqual "e1"
    DefinitionData.arrayEndMarker.toHex shouldEqual "f1"

  }

  test("Full") {
    val res: Either[BinCodecLibError, DefinitionData] = json.flatMap(DefinitionsDecoding.decodeDefinitions)
    getOrLog(res)
  }
  test("Test Decoding") {
    val decoded = json.flatMap(JsonUtils.decode(_, Decoder[DefinitionsDecoding.DefinitionJson]))
    getOrLog(decoded)

  }

}
