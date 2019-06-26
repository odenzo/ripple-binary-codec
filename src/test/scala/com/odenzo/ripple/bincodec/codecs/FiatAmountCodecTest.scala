package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import io.circe.syntax._
import org.scalatest.BeforeAndAfterAll
import scribe.Level
import spire.math.ULong

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.{Decoded, OTestSpec, RawValue, TestLoggingConfig}

class FiatAmountCodecTest extends OTestSpec with BeforeAndAfterAll with FiatAmountCodec {

  // Ripple method working, but we can make more concise using Java Math
  // This tries to compare the two.

  import com.odenzo.ripple.bincodec.syntax.debugging._

  override def beforeAll(): Unit = {
   TestLoggingConfig.debugLevel()
  }

  override def afterAll(): Unit = {
    TestLoggingConfig.setAll(Level.Warn)
  }

  test("Fiat Amount Test Cases") {
    val minMantissa: ULong = ULong("1000000000000000")
    val maxMantissa: ULong = ULong("9999999999999999")

    /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
    val minExponent: Int = -96
    val maxExponent: Int = 80

    val max: BigDecimal  = BigDecimal(maxMantissa * BigInt(10).pow(maxExponent))
    val zero: BigDecimal = BigDecimal(0)
    val min: BigDecimal  = BigDecimal(minMantissa / BigInt(10).pow(minExponent.abs))

    List(
      "123.123",
      "0000.111234",
      "1123.0",
      "11002200"
    ).map(v ⇒ BigDecimal(v)).map(ensureSame)

    def ensureSame(amount: BigDecimal) = {
      scribe.info(s"\n\nEnsuring Fiat Amount $amount")
      val v2: (ULong, Int) = getOrLog(newEncodeFiatAmount(amount))

    }

  }

  test("BigD Scaling") {

    BigInt(Long.MaxValue) > maxMantissa.toBigInt shouldBe true
    ULong.MaxValue > maxMantissa shouldBe true

    List(
      "123.123",
      "0000.00111234",
      "1123.0",
      "11002200",
    ).map(v ⇒ BigDecimal(v)).map(testOne)

  }

  def backToBigDecimal(mant: ULong, exp: Int): BigDecimal = {
    exp match {
      case ex if ex > 0 ⇒ BigDecimal(mant.toBigInt.bigInteger) * BigDecimal(10).pow(exp)
      case ex if ex < 0 ⇒ BigDecimal(mant.toBigInt.bigInteger) / BigDecimal(10).pow(exp.abs)
      case 0            ⇒ BigDecimal(mant.toBigInt.bigInteger)
    }
  }

  test("Overflow") {
    TestLoggingConfig.debugLevel()
    val src                                       = BigDecimal("999999999999999999999")
    val v: Either[RippleCodecError, (ULong, Int)] = newEncodeFiatAmount(src) // This is going to drop precision

    val (mant: ULong, exp: Int) = getOrLog(v)

    val looped = backToBigDecimal(mant, exp)

    scribe.debug(s"Source: $src  rounded to $looped")

    looped < src shouldBe true
  }

  test("Error Cases") {

    List(
      maxVal + 100,
      minVal - 100,
    ).map { bd: BigDecimal ⇒
      val v2: Either[RippleCodecError, (ULong, Int)] = newEncodeFiatAmount(bd)
      scribe.debug(s"New $v2")
      v2.isLeft shouldBe true
    }
  }

  test("Zero Boundary Cases") {
    TestLoggingConfig.setAll(Level.Debug)

    List(
      "0000.00000000",
    ).map(v ⇒ BigDecimal(v)).map(testOne)

  }

  test("Min Boundary ") {
    TestLoggingConfig.setAll(Level.Debug)
    BigInt(Long.MaxValue) > maxMantissa.toBigInt shouldBe true
    ULong.MaxValue > maxMantissa shouldBe true

    scribe.debug(s"Max Long: ${Long.MaxValue}  Max ULong ${ULong.MaxValue}")
    List(
      minAbsAmount.toString(),
    ).map(v ⇒ BigDecimal(v)).map(testOne)

  }

  test("Max") {
    TestLoggingConfig.setAll(Level.Debug)
    BigInt(Long.MaxValue) > maxMantissa.toBigInt shouldBe true
    ULong.MaxValue > maxMantissa shouldBe true

    scribe.debug(s"Max Long: ${Long.MaxValue}  Max ULong ${ULong.MaxValue}")
    List(
      maxAbsAmount.toString(),
    ).map(v ⇒ BigDecimal(v)).map { bd ⇒
      val v2: (ULong, Int) = getOrLog(newEncodeFiatAmount(bd))

    }

  }

  def testOne(bd: BigDecimal) {

    val v2: (ULong, Int) = getOrLog(newEncodeFiatAmount(bd))

  }

}
