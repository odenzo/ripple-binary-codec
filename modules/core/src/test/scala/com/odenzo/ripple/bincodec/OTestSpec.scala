package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.{Eval, _}
import io.circe.{Decoder, JsonObject}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should
import org.scalatest.{EitherValues, OptionValues}
import scodec.Attempt
import scribe.{Level, Logging}

import com.odenzo.ripple.bincodec.testkit.{OTestUtils, RippleTestUtils, TestLoggingConfig}
import com.odenzo.ripple.bincodec.utils.ScribeLoggingConfig

trait OTestSpec
    extends AnyFunSuiteLike
    with Logging
    with should.Matchers
    with EitherValues
    with OptionValues
    with OTestUtils
    with RippleTestUtils {

  import scodec.bits._

  def smartprint(a: Any) = {
    pprint.apply(a)

  }
  private val touch = TestLoggingConfig.setTestLogging.value

  def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") == "true"

  def setTestLog(l: Level): Unit = if (!inCI) ScribeLoggingConfig.setAllToLevel(l)

  def setLogDebug(): Unit = setTestLog(Level.Debug)

  def getOrLog[T](ee: Either[Throwable, T], msg: String = "Error: "): T = {
    ee match {
      case Right(v) => v
      case Left(emsg) =>
        scribe.error(s"$msg\t=> ${emsg} ")
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

  def expectSuccess[T](rs: Either[Exception, ByteVector])(fn: ByteVector => T): T = {
    rs match {
      case Left(err) =>
        scribe.error(s"Unexpected Failure: ${err} ", err)
        fail(err)
      case Right(v) =>
        scribe.debug(s"Result ${v.toHex}")
        fn(v)
    }
  }

  def expectFailure(rs: Either[Exception, ByteVector])(fn: Throwable => Any): Any = {
    rs match {
      case Left(err: Throwable) =>
        scribe.debug(s"Got Expceted Failure ${err}")
        fn(err)
      case Right(v) =>
        fail(s"Expected Failure But Got Result ${v.toHex}")
    }
  }

  def shouldSucceedAs[T](a: Attempt[T])(b: T) = shouldSucceed(a) shouldEqual (b)

  def shouldSucceed[T](a: Attempt[T]): T = a match {
    case Attempt.Failure(e)        => fail(e.messageWithContext)
    case Attempt.Successful(value) => value
  }

  def shouldFail[T](a: Attempt[T]): Unit = a match {
    case Attempt.Failure(e)        => scribe.info(s"Expected Failure $e")
    case Attempt.Successful(value) => fail(s"Should have failed but got $value")
  }
}
