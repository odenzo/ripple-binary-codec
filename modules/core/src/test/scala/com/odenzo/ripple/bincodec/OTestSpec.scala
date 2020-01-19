package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.Eval
import cats._
import io.circe.Decoder
import io.circe.JsonObject
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import scribe.Level
import scribe.Logging

import com.odenzo.ripple.bincodec.testkit.OTestUtils
import com.odenzo.ripple.bincodec.testkit.RippleTestUtils
import com.odenzo.ripple.bincodec.testkit.TestLoggingConfig
import com.odenzo.ripple.bincodec.utils.ScribeLoggingConfig

trait OTestSpec
    extends AnyFunSuiteLike
    with Logging
    with should.Matchers
    with EitherValues
    with OptionValues
    with OTestUtils
    with RippleTestUtils {
  private val touch = TestLoggingConfig.setTestLogging.value

  def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") == "true"

  def setTestLog(l: Level): Unit = if (!inCI) ScribeLoggingConfig.setAllToLevel(l)

  def setLogDebug(): Unit = setTestLog(Level.Debug)

  def getOrLog[T](ee: Either[Throwable, T], msg: String = "Error: "): T = {
    import com.odenzo.ripple.bincodec.BCException.showThrowables
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
