package com.odenzo.ripple.bincodec.reference

import scala.io.Source

import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.OTestSpec
import io.circe._
import io.circe.syntax._

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr

class DefinitionDataTest extends OTestSpec {

  val resourceName = "/ripplereferencedata/definitions.json"
  scribe.info(s"Loading Default Data from $resourceName")

  val json: ErrorOr[Json] = Option(this.getClass.getResourceAsStream(resourceName)) match {
    case None => RippleCodecError(s"Couldn't Find Definitions Resource $resourceName").asLeft
    case Some(s) =>
      val txt = Source.fromInputStream(s, "UTF-8").getLines().mkString("\n")
      JsonUtils.parseAsJson(txt)
  }

  test("Constants") {
    DefinitionData.pathSetEnd.toHex shouldEqual "00"
    DefinitionData.pathSetAnother.toHex shouldEqual "FF"
    DefinitionData.objectEndMarker.toHex shouldEqual "E1"
    DefinitionData.arrayEndMarker.toHex shouldEqual "F1"

  }

  test("Full") {
    super.setLogToDebug()
    val res: Either[RippleCodecError, DefinitionData] = json.flatMap(DefinitionsDecoding.decodeDefinitions)
    val ok                                            = getOrLog(res)
    logger.debug(s"FILTERED AND MAPPED: ${ok.fieldsInfo.size}")
  }
  test("Test Decoding") {
    super.setLogToDebug()
    logger.warn("Loading")
    val decoded = json.flatMap(JsonUtils.decode(_, Decoder[DefinitionsDecoding.DefinitionJson]))
    logger.debug(s"Decoded $decoded")
    val ok = getOrLog(decoded)
    logger.debug(s"OK: ${ok.fields.size}")

  }

}
