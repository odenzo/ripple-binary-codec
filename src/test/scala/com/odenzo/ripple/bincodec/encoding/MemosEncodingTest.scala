package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import org.scalatest.{Assertion, FunSuite}

import com.odenzo.ripple.bincodec.codecs.MoneyCodecs
import com.odenzo.ripple.bincodec.{OTestSpec, OTestUtils, RawValue}

/**
  * Trouble with Fiat encoding so a dedicated test suite.
  */
class MemosEncodingTest extends FunSuite with OTestSpec with OTestUtils {



  def testOne(v: Json, expected: Json): Assertion = {
    val expectedHex                             = expected.asString.get
    val bytes: RawValue = getOrLog(MoneyCodecs.encodeFiatValue(v))
    bytes.toHex shouldEqual expectedHex
  }

}
