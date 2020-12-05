package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.{Eval, _}
import io.circe.{Decoder, Json, JsonObject}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should
import org.scalatest.{EitherValues, OptionValues}
import scodec.{Attempt, DecodeResult}
import scribe.{Level, Logging}

import com.odenzo.ripple.bincodec.setup.Setup
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

  def decodeJson[T: Decoder](json: Json): T = {
    Decoder[T].decodeJson(json) match {
      case Left(value)  => fail(s"Failed Decoding ${json.spaces2}: $value")
      case Right(value) => value
    }
  }

  def roundTripScodec[T](tp: TestParam[T], codec: scodec.Codec[T])(implicit log: Boolean): DecodeResult[T] = {
    val decResult = roundTripScodec(tp.binary, tp.model, codec)
    if (tp.consumesAll) decResult.remainder.isEmpty shouldBe true
    decResult
  }

  def roundTripScodec[T](binary: BitVector, model: T, codec: scodec.Codec[T])(implicit log: Boolean): DecodeResult[T] = {
    val encoded: BitVector = codec.encode(model).require
    if (log) {
      scribe.debug(s"Given:\n $binary \n And: $model \n Codec: $codec")
      scribe.debug(s"Encoded: $encoded")

    }
    val decoded: DecodeResult[T]      = codec.decode(binary).require
    val roundDecoded: DecodeResult[T] = codec.decode(encoded).require

    if (log) {
      scribe.debug(s"Decoded: $decoded")
    }
    encoded shouldEqual binary
    decoded.value shouldEqual model
    roundDecoded.value shouldEqual decoded.value
    decoded
  }

  /** Ensures all bits are exhausted on decoding back to value */
  def roundTripFromEncode[T](model: T, theCodec: scodec.Codec[T])(implicit log: Boolean): BitVector = {
    val codec = theCodec
    if (log) scribe.debug(s"Given:\n $model \n Codec: $codec")

    val bits = codec.encode(model).require
    if (log) scribe.debug(s"Encoded: $bits")

    val back = codec.decode(bits).require
    if (log) scribe.debug(s"Decoded: $back")

    back.value shouldEqual model
    back.remainder.isEmpty shouldBe true
    bits
  }

  case class TestParam[T](binary: BitVector, model: T, consumesAll: Boolean = true)

  Setup.init()
}
