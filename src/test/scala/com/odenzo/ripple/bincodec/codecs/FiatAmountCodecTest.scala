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
import com.odenzo.ripple.bincodec.{Decoded, OTestSpec}

class FiatAmountCodecTest extends OTestSpec with BeforeAndAfterAll with FiatAmountCodec {

  // Ripple method working, but we can make more concise using Java Math
  // This tries to compare the two.

  import com.odenzo.ripple.bincodec.syntax.debugging._

  override def beforeAll(): Unit = {
    if (!com.odenzo.ripple.bincodec.inCI) {
      com.odenzo.ripple.bincodec.setAllToLevel(Level.Debug)
    }
  }

  override def afterAll(): Unit = {
    if (!com.odenzo.ripple.bincodec.inCI) {
      com.odenzo.ripple.bincodec.setAllToLevel(Level.Warn)
    }
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
      val mine: (ULong, Int) = oldPrenormalize(amount)
      val myNorm  = getOrLog(myNormalize(mine._1,mine._2))
      val norm: (ULong, Int) = getOrLog(calcNormalizedMantissaAndExp(amount))
      val exposed: Decoded   = getOrLog(rippleEncodingOfFiatAmount(amount))
      myNorm shouldEqual norm

    }

  }

  test("BigD Scaling") {

    BigInt(Long.MaxValue) > maxMantissa.toBigInt shouldBe true
    ULong.MaxValue > maxMantissa shouldBe true

    scribe.debug(s"Max Long: ${Long.MaxValue}  Max ULong ${ULong.MaxValue}")
    List(
      "123.123",
      "0000.00111234",
      "1123.0",
      "11002200",
    ).map(v ⇒ BigDecimal(v)).map(testOne)

  }

  test("Error Cases") {
    List(
         
          "123456789.123456789123", // Too much precision as underflow (rehect)
          "123456789012345678900" // Too much precision that significantly effects amount (reject)
          ).map(v ⇒ BigDecimal(v)).map(testOne)
  }

  test("Boundary Cases") {

    BigInt(Long.MaxValue) > maxMantissa.toBigInt shouldBe true
    ULong.MaxValue > maxMantissa shouldBe true

    scribe.debug(s"Max Long: ${Long.MaxValue}  Max ULong ${ULong.MaxValue}")
    List(
          "0000.00000000",
          minAbsAmount.toString(),
          maxAbsAmount.toString(),

          ).map(v ⇒ BigDecimal(v)).map(testOne)

  }

  def testOne(bd: BigDecimal) {
    val exp    = bd.scale
    val asLong = bd * BigDecimal(10).pow(exp)

    scribe.debug(s"BD: $bd with Scale: $exp ::  Scaled To $asLong  is valid long: ${asLong.isValidLong}")

    val (cman, cexp) = oldPrenormalize(bd)
    scribe.debug(s"Calculate Normalized: $cman * 10^ $cexp")
    asLong.toLong

  }


}
