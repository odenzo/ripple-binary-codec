package com.odenzo.ripple.bincodec.utils

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.OTestSpec

class JsonUtilsTest extends FunSuite with OTestSpec with JsonUtils {

  val badJson = " feofiwjeoifjoe"

  val aJson =
    """
      |{
      | "a" : "1a",
      | "b" : "1b"
      | }
      |
    """.stripMargin

  val bJson =
    """
      |{
      | "b" : "2b" ,
      | "c" : "2c"
      | }
      |
    """.stripMargin

  test("NEG Bad Json Parse") {

    JsonUtils.parseAsJson(badJson).isLeft shouldBe true
  }

  test("Monoid") {
    val sum = for {
      a <- parseAsJsonObject(aJson)
      b <- parseAsJsonObject(bJson)

    } yield a |+| b

    val s = getOrLog(sum)
    scribe.info(s"Sum  ${s.asJson.spaces4}")

    s("b").value shouldEqual "2b".asJson
    s.size shouldEqual 3
  }

}
