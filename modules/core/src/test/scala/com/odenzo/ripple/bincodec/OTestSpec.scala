package com.odenzo.ripple.bincodec

import cats.Eval
import io.circe.{Decoder, JsonObject}
import org.scalatest.{EitherValues, FunSuiteLike, Matchers, OptionValues}
import scribe.Logging

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError

trait OTestSpec extends FunSuiteLike with Logging with Matchers with EitherValues with OptionValues with OTestUtils {

  private val touch = TestLoggingConfig.setTestLogging.value

  def getOrLog[T](ee: ErrorOr[T], msg: String = "Error: "): T = {

    RippleCodecError.dump(ee) match {
      case None       => //scribe.debug("No Errors Found")
      case Some(emsg) => scribe.error(s"$msg\t=> $emsg ")
    }

    ee match {
      case Left(e)  => assert(false, "getOrLog error"); throw new Exception("getOrLogError")
      case Right(v) => v
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

}
