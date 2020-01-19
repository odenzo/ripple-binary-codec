package com.odenzo.ripple.bincodec.utils

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.OTestSpec

class JsonUtilsTest extends OTestSpec with JsonUtils {

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

}
