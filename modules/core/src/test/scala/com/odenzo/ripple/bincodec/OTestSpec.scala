package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.{Eval, _}
import io.circe.{Json, JsonObject, Decoder}
import org.scalatest.{OptionValues, FunSuiteLike, EitherValues, Matchers}
import scribe.Logging

import com.odenzo.ripple.bincodec.testkit.{TestLoggingConfig, RippleTestUtils, OTestUtils}

trait OTestSpec
    extends FunSuiteLike
    with Logging
    with Matchers
    with EitherValues
    with OptionValues
    with OTestUtils
    with RippleTestUtils {

  private val touch = TestLoggingConfig.setTestLogging.value

  def getOrLog[T](ee: Either[Throwable, T], msg: String = "Error: "): T = {
    import BCException.showThrowables
    ee match {
      case Right(v) => v
      case Left(emsg) =>
        scribe.error(s"$msg\t=> ${emsg.show} ")
        fail(emsg)
    }
  }

  def findRequiredStringField(name: String, jobj: JsonObject): String = {
    getOrLog(findField(name, jobj).flatMap(json2string))
  }

  /**
    *
    * @return Json of field or logging of error and assertion failure
    */
  def findRequiredField(name: String, json: JsonObject): Json = {
    getOrLog(findField(name, json))
  }

  def findRequiredObject(name: String, jsonObject: JsonObject): JsonObject = {
    val asObj = findField(name, jsonObject).flatMap(json2object)
    getOrLog(asObj)
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
