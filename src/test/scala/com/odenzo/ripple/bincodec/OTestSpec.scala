package com.odenzo.ripple.bincodec

import java.net.URL
import scala.io.{BufferedSource, Source}

import io.circe.{Decoder, Json, JsonObject}
import org.scalatest.{EitherValues, FunSuiteLike, Matchers, OptionValues}
import scribe.Level

import com.odenzo.ripple.bincodec
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, RippleCodecError}

trait OTestSpec extends FunSuiteLike with Matchers with EitherValues with OptionValues with OTestUtils {

  val touch  = TestLoggingConfig.setTestLogging

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

}
