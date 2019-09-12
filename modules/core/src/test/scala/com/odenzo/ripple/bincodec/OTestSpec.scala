package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.{Eval, _}
import io.circe.{JsonObject, Decoder}
import org.scalatest.{OptionValues, FunSuiteLike, EitherValues, Matchers}
import scribe.Logging

import ErrorOr.ErrorOr

trait OTestSpec extends FunSuiteLike with Logging with Matchers with EitherValues with OptionValues with OTestUtils {

  private val touch = TestLoggingConfig.setTestLogging.value

  def getOrLog[T](ee: ErrorOr[T], msg: String = "Error: "): T = {
    ee match {
      case Right(v) => v
      case Left(emsg) =>
        scribe.error(s"$msg\t=> ${emsg.show} ")
        fail(emsg)
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
