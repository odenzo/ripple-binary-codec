package com.odenzo.ripple.bincodec

import java.net.URL
import scala.io.{Source, BufferedSource}

import cats._
import cats.data._
import cats.implicits._
import io.circe.optics.JsonPath.root
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import monocle.Optional
import scribe.Level

import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait OTestUtils extends JsonUtils {

  /**
    * This will load from resources/test/fixtures/...
    * Most of those were stolen from Ripple Javascript.
    *
    * @param in  JSON File Name as input to a test fixture
    * @param out JSON File Name matching the desired result
    */
  def loadFixture(in: String, out: String): ErrorOr[(Json, Json)] = {

    for {
      inJson <- loadJsonResource(s"/test/fixtures/$in")
      okJson <- loadJsonResource(s"/test/fixtures/$out")
    } yield (inJson, okJson)

  }

  def loadJsonResource(path: String): Either[BinCodecLibError, Json] = {
    BinCodecLibError.wrap(s"Getting Resource $path") {
      val resource: URL          = getClass.getResource(path)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }

  val rs_resultLens: Optional[Json, JsonObject] = root.result.obj
  val rs_txjsonLens: Optional[Json, JsonObject] = root.result.tx_json.obj

  def findResult(json: Json): Either[BinCodecLibError, JsonObject] =
    rs_resultLens.getOption(json).toRight(BinCodecLibError("result obj not found", json))

  def findResult(obj: JsonObject): Either[BinCodecLibError, JsonObject] = findFieldDeepAsObject("result", obj.asJson)
  def findTxJson(obj: JsonObject): Either[BinCodecLibError, JsonObject] = findFieldDeepAsObject("tx_json", obj.asJson)
  def findResultTxBlob(obj: JsonObject): Either[BinCodecLibError, String] = {
    findResult(obj).flatMap(findField("tx_blob", _)).flatMap(json2string)
  }

  def setLogToDebug(): Unit = TestLoggingConfig.setAll(Level.Debug)
  def setLogToWarn(): Unit  = TestLoggingConfig.setAll(Level.Warn)

}
