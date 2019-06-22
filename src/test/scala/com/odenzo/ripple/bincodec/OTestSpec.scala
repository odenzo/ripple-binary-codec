package com.odenzo.ripple.bincodec

import java.net.URL
import scala.io.{BufferedSource, Source}

import io.circe.{Decoder, Json, JsonObject}
import org.scalatest.{EitherValues, Matchers, OptionValues}
import scribe.Level

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, RippleCodecError}

trait OTestSpec extends Matchers with EitherValues with OptionValues {

  // Setting Global Levels...I am using global logger everywhere
  scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Warn)).replace()
  /**
    * This will load from resources/test/fixtures/...
    * Most of those were stolen from Ripple Javascript.
    *
    * @param in JSON File Name as input to a test fixture
    * @param out JSON File Name matching the desired result
    */
  def loadFixture(in: String, out: String): ErrorOr[(Json, Json)] = {

    for {
      inJson <- loadJsonResource(s"/test/fixtures/$in")
      okJson ← loadJsonResource(s"/test/fixtures/$out")
    } yield (inJson, okJson)

  }

  def loadJsonResource(path: String): Either[RippleCodecError, Json] = {
    BinCodecExeption.wrap(s"Getting Resource $path") {
      val resource: URL          = getClass.getResource(path)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }

  def getOrLog[T](ee: ErrorOr[T], msg: String = "Error: "): T = {
    if (ee.isLeft) {
      RippleCodecError.dump(ee) match {
        case None       ⇒ scribe.debug("No Errors Found")
        case Some(emsg) ⇒ scribe.error(s"$msg\t=> $emsg ")
      }
      assert(false, s"Auto Test of $msg")

    }
    ee.right.value
  }

}

/**
  * Common to have object with binary and json in test files.
  * @param binary
  * @param json
  */
case class TestFixData(json: JsonObject, binary: String)

object TestFixData {

  import io.circe.generic.semiauto.deriveDecoder

  implicit val decoder: Decoder[TestFixData] = deriveDecoder[TestFixData]
}
