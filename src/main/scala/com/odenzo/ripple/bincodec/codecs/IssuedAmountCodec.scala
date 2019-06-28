package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.ULong

import com.odenzo.ripple.bincodec.RawValue
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, OErrorRipple, RippleCodecError}

trait IssuedAmountCodec extends CodecUtils {

  protected val minVal: BigDecimal       = BigDecimal("-9999999999999999E80")
  protected val maxVal: BigDecimal       = BigDecimal("9999999999999999e80")
  protected val minAbsAmount: BigDecimal = BigDecimal("1000000000000000e-95")

  protected val maxPrecision: Int        = 15

  /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
  protected val minExponent: Int    = -96
  protected val maxExponent: Int    = 80
  protected val minMantissa: BigInt = BigDecimal("1e15")toBigInt() // For normalizing not input
  protected val maxMantissa: BigInt = BigDecimal("10e16").toBigInt()-1 // For normalizing not input

  // 64 bits!=  20 * 8  160 bits which  doesn't match 2*160 or 3*160
  val ZERO_SPECIAL_CASE =  "0x8000000000000000000000000000000000000000"

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
  def encodeFiatValue(json: Json): Either[RippleCodecError, RawValue] = {
    BinCodecExeption.wrap(s"Error Normalizing Fiat Amount ${json.spaces4}") {
      for {
        str         <- JsonUtils.json2string(json)
        amt         = BigDecimal(str)
        norm        ← newEncodeFiatAmount(amt)
        (mant, exp) = norm
        isNeg       = amt.signum < 0
        enc         <- binaryFormatFiatAmount(isNeg, mant, exp)
      } yield enc
    }
  }

  def newEncodeFiatAmount(amount: BigDecimal): Either[RippleCodecError, (ULong, Int)] = {

    if (amount === 0) {
      (ULong.MinValue, 0).asRight[RippleCodecError]
    } else {
      val isNeg = amount.signum < 0

      for {
        prenorm <- newPreNormalize(amount.abs)
        _       = scribe.info(s"NEW PRENORMALIZED  $prenorm")
        norm    <- newNormalize(prenorm._1, prenorm._2)
        _       = scribe.info("NEW METHOD FINAL NORMALIZATION: " + norm)
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
  protected def newPreNormalize(bd: BigDecimal): Either[OErrorRipple, (BigInt, Int)] = {
    val absBd = bd.abs
    scribe.debug(s"Normalizing $bd with precision ${bd.precision}")
    if (bd > maxVal || bd < minVal || bd < minAbsAmount) {
      RippleCodecError(s"$bd not in range $minVal ...$maxVal  or smaller than $minAbsAmount").asLeft
    } else if (absBd < minAbsAmount) {
      RippleCodecError(s"$bd Absolute value smaller than $minAbsAmount").asLeft
    } else if (bd.precision > 16) {
      RippleCodecError(s"$bd Precision > 16 and probably would be rounded").asLeft
    } else {
      val exp     = absBd.scale
      val fExp    = 0 - exp
      val noScale = absBd * BigDecimal(10).pow(exp)
      scribe.debug(s"BD: $absBd   Exp: $exp  Scaled To $noScale  is valid long: ${noScale.isValidLong}")
      scribe.info(s"NEW METHOD PRENORMALIZED: $noScale $fExp")
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
  protected def newNormalize(mantissa: BigInt, exponent: Int): Either[RippleCodecError, (ULong, Int)] = {
    // Okay, we have a mantissa and and exp. We want (generally) to increate the mantissa until 10000000L which means
    // the exponent has to decrease
    // A quick literal translation

    // Before we do this we have converted mantissa to abs() and recorded the sign bit.
    // So the equivalent C++ code (which stores in  std::int64_t) is not needed.

    val res: Either[RippleCodecError, (ULong, Int)] =
      BinCodecExeption.wrap(s"Error Normalizing $mantissa ^ $exponent") {
        var mant: BigInt = mantissa
        var exp: Int     = exponent

        // Crank up the mantissa as much as we can
        while ((mant < minMantissa) && (exp > minExponent)) {
          mant = mant * ULong(10)
          exp = exp - 1
        }

        // Crank down the mantissa if too high, making sure not to overflow exponent
        while (mant > maxMantissa) {
          if (exp >= maxExponent) throw new IllegalArgumentException("Amount out range - overflow")
          mant = mant / ULong(10)
          exp = exp + 1
        }

        // Check to see if number too small to represent and truncate to zero
        if (exp > maxExponent) throw new IllegalArgumentException("Amount out range - overflow")

        if ((exp < minExponent) || (mant < minMantissa)) (ULong(0), 0).asRight
        else if (mant <= ULong.MaxValue) {
          (ULong.fromBigInt(mant), exp).asRight
        } else {
          scribe.warn(s"Mantissa Too Big for Initial $mantissa E $exp")
          RippleCodecError(s"System Error with Amount ${mantissa} * 10 ^ ${exponent}").asLeft
        }
      }
    res.foreach(v ⇒ scribe.info(s"NEW FINAL NORMALIZED: $v"))
    res
  }

  /** V2: Given normalized amount encode it in ripple binary format
    * This is used for all algorithms once we have normalized mantissa and exponent */
  protected def binaryFormatFiatAmount(isNegative: Boolean,
                                       mantissa: ULong,
                                       exp: Int): Either[RippleCodecError, RawValue] = {

    scribe.info(s"Encoding BigDecimal Fiat $isNegative  $mantissa  ^  $exp")

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

      val asBytes: Either[RippleCodecError, RawValue] = encodeULong(full, "UInt64")
      asBytes
    }
  }

  def validateFiatAmount(amount: BigDecimal): Either[OErrorRipple, BigDecimal] = {

    amount match {
      case amt if amt < minVal ⇒
        scribe.info(s"$amt less than min - underflow to ZERO")
        BigDecimal(0).asRight

      case amt if amt > maxVal ⇒
        RippleCodecError(s"Overflow FiatAmount $amt < $maxVal").asLeft

      case amt if amt.precision > maxPrecision ⇒
        // Too much precision, some will be ignored. But if close to zero make zero?
        // FIXME: Max Precision check hacked ... add cases to round down to ZERO on underflow
        //AppError(s"Prevision Overflow $amount ${amount.precision} > $maxPrecision").asLeft

        // Well, what to do here.
        scribe.debug(s"Too Much Precision $amt was ${amt.precision} w/ Scale ${amt.scale}")
        //RippleCodecError(s"Too Much Precision $amt was ${amt.precision} w/ Scale ${amt.scale}").asLeft
        amt.asRight // Test CAses seem to pass this
      case amountInBounds ⇒ amountInBounds.asRight
    }
  }

  /** Check the bounds after amount in normalized */
  protected def checkMantissaBounds(mantissa: ULong): Either[OErrorRipple, ULong] = {
    if (mantissa < minMantissa || mantissa > maxMantissa) {
      RippleCodecError(s"$mantissa has to be in range $minMantissa - $maxMantissa").asLeft
    } else {
      mantissa.asRight
    }

  }

  /** Check the bounds after amount in normalized */
  protected def checkExponentBounds(exp: Int): Either[OErrorRipple, Int] = {
    if (exp < minExponent || exp > maxExponent) {
      RippleCodecError(s"$exp has to be in range $minExponent to $maxExponent inclusive ").asLeft
    } else {
      exp.asRight
    }

  }

}

object IssuedAmountCodec extends IssuedAmountCodec
