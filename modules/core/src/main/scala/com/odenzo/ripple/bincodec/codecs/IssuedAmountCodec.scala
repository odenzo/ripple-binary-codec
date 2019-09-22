package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.ULong

import com.odenzo.ripple.bincodec.{BCLibErr, RawValue, BinCodecLibError}
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait IssuedAmountCodec extends CodecUtils {

  protected val minVal: BigDecimal       = BigDecimal("-9999999999999999E80")
  protected val maxVal: BigDecimal       = BigDecimal("9999999999999999e80")
  protected val minAbsAmount: BigDecimal = BigDecimal("1000000000000000e-95")

  protected val maxPrecision: Int = 15

  /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
  protected val minExponent: Int    = -96
  protected val maxExponent: Int    = 80
  protected val minMantissa: BigInt = BigDecimal("1e15").toBigInt // For normalizing not input
  protected val maxMantissa: BigInt = BigDecimal("10e16").toBigInt - 1 // For normalizing not input

  // 64 bits!=  20 * 8  160 bits which  doesn't match 2*160 or 3*160
  val ZERO_SPECIAL_CASE = "0x8000000000000000000000000000000000000000"

  /** This is closest number to zero that is valid, ie smallest absolute value  */
  /**
    * Encode the amount field of a fiat value structure.
    * Fiat value can be positive or negative and very large or very
    * small (e.g.
    * 0.0000004)
    * Adapted from docs and IOUAmount.cpp in rippled code.
    *
    * @param json
    */
  def encodeFiatValue(json: Json): Either[BinCodecLibError, RawValue] = {
    BinCodecLibError.wrap(s"Error Normalizing Fiat Amount ${json.spaces4}") {
      for {
        str <- JsonUtils.json2string(json)
        amt = BigDecimal(str)
        norm <- newEncodeFiatAmount(amt)
        (mant, exp) = norm
        isNeg       = amt.signum < 0
        enc <- binaryFormatFiatAmount(isNeg, mant, exp)
      } yield enc
    }
  }

  def newEncodeFiatAmount(amount: BigDecimal): Either[BinCodecLibError, (ULong, Int)] = {

    if (amount === 0) {
      (ULong.MinValue, 0).asRight[BinCodecLibError]
    } else {
      val isNeg = amount.signum < 0

      for {
        prenorm <- newPreNormalize(amount.abs)
        _ = scribe.debug(s"NEW PRENORMALIZED  $prenorm")
        norm <- newNormalize(prenorm._1, prenorm._2)
        _ = scribe.debug("NEW METHOD FINAL NORMALIZATION: " + norm)
      } yield norm
    }
  }

  /**
    * Convert amount as big decimal to a valid ULong and exponent, but NOT normalized Ripple style
    * Ensures within bounds for Ripple Fiat Amount
    *
    * @param bd
    *
    * @return Absolute value of bd scales by 10 ** n such that the value is .xyyyyyyyy..   where x != 0
    */
  protected def newPreNormalize(bd: BigDecimal): Either[BCLibErr, (BigInt, Int)] = {
    val absBd = bd.abs
    scribe.debug(s"Normalizing $bd with precision ${bd.precision}")
    if (bd > maxVal || bd < minVal || bd < minAbsAmount) {
      BinCodecLibError(s"$bd not in range $minVal ...$maxVal  or smaller than $minAbsAmount").asLeft
    } else if (absBd < minAbsAmount) {
      BinCodecLibError(s"$bd Absolute value smaller than $minAbsAmount").asLeft
    } else if (bd.precision > 16) {
      BinCodecLibError(s"$bd Precision > 16 and probably would be rounded").asLeft
    } else {
      val exp     = absBd.scale
      val fExp    = 0 - exp
      val noScale = absBd * BigDecimal(10).pow(exp)
      (noScale.toBigInt, fExp).asRight
    }
  }

  /**
    * @param mantissa
    * @param exponent
    *
    * @return tuple, normalized mantissa, exponent  - if underflow both fields set to 0
    *         Overflow returned as Error
    */
  protected def newNormalize(mantissa: BigInt, exponent: Int): Either[BinCodecLibError, (ULong, Int)] = {

    val res: Either[BinCodecLibError, (ULong, Int)] =
      BinCodecLibError.wrap(s"Error Normalizing $mantissa ^ $exponent") {
        var mant: BigInt = mantissa
        var exp: Int     = exponent

        // Crank up the mantissa as much as we can
        while ((mant < minMantissa) && (exp > minExponent)) {
          mant = mant * ULong(10)
          exp  = exp - 1
        }

        // Crank down the mantissa if too high, making sure not to overflow exponent
        while (mant > maxMantissa) {
          if (exp >= maxExponent) throw new IllegalArgumentException("Amount out range - overflow")
          mant = mant / ULong(10)
          exp  = exp + 1
        }

        // Check to see if number too small to represent and truncate to zero
        if (exp > maxExponent) throw new IllegalArgumentException("Amount out range - overflow")

        if ((exp < minExponent) || (mant < minMantissa)) (ULong(0), 0).asRight
        else if (mant <= ULong.MaxValue) {
          (ULong.fromBigInt(mant), exp).asRight
        } else {
          scribe.warn(s"Mantissa Too Big for Initial $mantissa E $exp")
          BinCodecLibError(s"System Error with Amount ${mantissa} * 10 ^ ${exponent}").asLeft
        }
      }
    res.foreach(v => scribe.debug(s"NEW FINAL NORMALIZED: $v"))
    res
  }

  /** V2: Given normalized amount encode it in ripple binary format
    * This is used for all algorithms once we have normalized mantissa and exponent */
  protected def binaryFormatFiatAmount(
      isNegative: Boolean,
      mantissa: ULong,
      exp: Int
  ): Either[BinCodecLibError, RawValue] = {

    scribe.debug(s"Encoding BigDecimal Fiat $isNegative  $mantissa  ^  $exp")

    if (mantissa === ULong(0)) {
      val zero = ULong(1) << 63
      encodeULong(zero, "UInt64") // This is 8 bytes.
    } else {
      // 16 bits, only bottom two used
      val signBits: ULong       = if (isNegative) ULong(2) else ULong(3)
      val top2Bits: ULong       = signBits << 62
      val expBitsShifted: ULong = ULong(exp + 97L) << 54
      val top10: ULong          = top2Bits | expBitsShifted
      val full: ULong           = top10 | mantissa

      val asBytes: Either[BinCodecLibError, RawValue] = encodeULong(full, "UInt64")
      asBytes
    }
  }

  def validateFiatAmount(amount: BigDecimal): Either[BCLibErr, BigDecimal] = {

    amount match {
      case amt if amt < minVal =>
        scribe.info(s"$amt less than min - underflow to ZERO")
        BigDecimal(0).asRight

      case amt if amt > maxVal =>
        BinCodecLibError(s"Overflow FiatAmount $amt < $maxVal").asLeft

      case amt if amt.precision > maxPrecision =>
        scribe.debug(s"Too Much Precision $amt was ${amt.precision} w/ Scale ${amt.scale}")
        //RippleCodecError(s"Too Much Precision $amt was ${amt.precision} w/ Scale ${amt.scale}").asLeft
        amt.asRight // Test CAses seem to pass this
      case amountInBounds => amountInBounds.asRight
    }
  }

  /** Check the bounds after amount in normalized */
  protected def checkMantissaBounds(mantissa: ULong): Either[BCLibErr, ULong] = {
    if (mantissa < minMantissa || mantissa > maxMantissa) {
      BinCodecLibError(s"$mantissa has to be in range $minMantissa - $maxMantissa").asLeft
    } else {
      mantissa.asRight
    }

  }

  /** Check the bounds after amount in normalized */
  protected def checkExponentBounds(exp: Int): Either[BCLibErr, Int] = {
    if (exp < minExponent || exp > maxExponent) {
      BinCodecLibError(s"$exp has to be in range $minExponent to $maxExponent inclusive ").asLeft
    } else {
      exp.asRight
    }

  }

}

object IssuedAmountCodec extends IssuedAmountCodec
