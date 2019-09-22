package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.{Eval, _}
import io.circe.{Json, JsonObject, Decoder}
import org.scalatest.{OptionValues, FunSuiteLike, EitherValues, Matchers}
import scribe.{Logging, Level}

import com.odenzo.ripple.bincodec.testkit.{TestLoggingConfig, RippleTestUtils, OTestUtils}
import com.odenzo.ripple.bincodec.utils.ScribeLoggingConfig

trait OTestSpec
    extends FunSuiteLike
    with Logging
    with Matchers
    with EitherValues
    with OptionValues
    with OTestUtils
    with RippleTestUtils {

  private val touch = TestLoggingConfig.setTestLogging.value

  def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") == "true"

  def setTestLog(l: Level): Unit = if (!inCI) ScribeLoggingConfig.setAllToLevel(l)

  def setLogDebug(): Unit = setTestLog(Level.Debug)

  def getOrLog[T](ee: Either[Throwable, T], msg: String = "Error: "): T = {
    import BCException.showThrowables
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
