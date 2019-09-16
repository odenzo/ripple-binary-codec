package com.odenzo.ripple.bincodec.testkit

import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr
import io.circe.{Json, Decoder}
import java.net.URL
import scala.io.{Source, BufferedSource}

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

  def loadRequestResponseFixture(path: String): Either[BinCodecLibError, List[JsonReqRes]] = {
    loadJsonResource(path).flatMap(json => decode(json, Decoder[List[JsonReqRes]]))
  }

  def loadJsonResource(path: String): Either[BinCodecLibError, Json] = {
    BinCodecLibError.wrap(s"Getting Resource $path") {
      val resource: URL          = getClass.getResource(path)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }

}

object OTestUtils extends OTestUtils
