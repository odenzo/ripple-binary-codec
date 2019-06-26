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
trait FiatAmountCodec extends CodecUtils {

  val minVal: BigDecimal = BigDecimal("-9999999999999999E80") ///?!?
  val maxVal: BigDecimal = BigDecimal("9999999999999999e80")
  val maxPrecision: Int  = 15

  /*
  Minimum nonzero absolute value: 1000000000000000e-96
  Maximum value: 9999999999999999e80
  Minimum value: -9999999999999999e80
  15 (16?) decimal digits of precision

   */

  /* The range for the mantissa when normalized */
  protected val minMantissa: ULong = ULong("1000000000000000")
  protected val maxMantissa: ULong = ULong("9999999999999999")

  /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
  protected val minExponent: Int = -96
  protected val maxExponent: Int = 80

  // Note: Can still have too much precision -- need to detect and handle as ???
  protected val maxAbsAmount: BigDecimal = BigDecimal(maxMantissa * BigInt(10).pow(maxExponent))
  protected val minAbsAmount: BigDecimal = BigDecimal(minMantissa / BigInt(10).pow(minExponent.abs))

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
    JsonUtils
      .json2string(json)
      .map(BigDecimal(_))
      .flatMap(encodeFiatAmount)

  }

  def encodeFiatAmount(amount: BigDecimal): Either[RippleCodecError, RawValue] = {
    val isNegative = amount.signum < 0
    val absAmount  = amount.abs

    val doOld = true

    val normalized = if (!doOld) {
      // My Attempt
      newNormalizeToIntegral(absAmount)
    } else {
      // Ripple
      calcNormalizedMantissaAndExp(amount)
    }

    normalized.flatMap {
      case (man: ULong, exp: Int) ⇒
        binaryFormatFiatAmount(isNegative, man, exp)
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
  protected def newNormalizeToIntegral(bd: BigDecimal): Either[RippleCodecError, (ULong, Int)] = {
    val absBd = bd.abs
    scribe.debug(s"Normalizing $bd with precision ${bd.precision}")
    if (absBd > maxAbsAmount || absBd < minAbsAmount) {
      RippleCodecError(s"$bd not in range $minAbsAmount ... " + s"$maxAbsAmount").asLeft
    } else {
      val exp     = absBd.scale
      val noScale = absBd * BigDecimal(10).pow(exp)
      scribe.debug(s"BD: $absBd   Exp: $exp  Scaled To $noScale  is valid long: ${noScale.isValidLong}")
      if (noScale.isValidLong) {
        val mantissa = ULong.fromLong(noScale.toLongExact)
        myNormalize(mantissa, exp)
      } else {
        RippleCodecError(s"Mantissa Range / Program Error $bd").asLeft
      }

    }
  }

  /**
    * @param mantissa
    * @param exponent
    *
    * @return tuple, normalized mantissa, exponent  - if underflow both fields set to 0
    *         Overflow returned as Error
    */
  protected def myNormalize(mantissa: BigInt, exponent: Int): Either[RippleCodecError, (ULong, Int)] = {
    // Okay, we have a mantissa and and exp. We want (generally) to increate the mantissa until 10000000L which means
    // the exponent has to decrease
    // A quick literal translation

    // Before we do this we have converted mantissa to abs() and recorded the sign bit.
    // So the equivalent C++ code (which stores in  std::int64_t) is not needed.

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
        RippleCodecError(s"System Error with Amount ${mantissa} * 10 ^ ${exponent}").asLeft
      }
    }

  }

  protected def calcNormalizedMantissaAndExp(bd: BigDecimal): Either[RippleCodecError, (ULong, Int)] = {
    /*
     * - Final value has to shift mantissa to range "1000000000000000"  "9999999999999999"
     * - Final value has to have exponenet in range -97 to 80 (which is later encoded as exp+97 to UByte 0 - 177
     *
     * * Quickly use the Ripple code copied from CPP but first need to make mantissa fit in ULong (64 bit)
     *
     * * Approach:
     * - first we have a number of any size and precision
     * - Use BigDecimal to shift the decimal point, given us mantissa and an exponent
     * - The mantissa has to fit in a ULong (64-bits)
     *
     * Above is shifted to normalizeToIntegral,
     *
     */
    // FIXME: Takes absolute value which is incorrect?, need to add back sign bit in encodings

    val (amount: ULong, exp: Int) = oldPrenormalize(bd.abs)
    scribe.info(s"Pre Normalized Mantissa $amount  Exponent $exp ")
    for {
      rippleNorm <- rippleNormalizeScale(amount, exp)
      _          = scribe.info("Ripple Normalized: " + rippleNorm)
    } yield rippleNorm
  }

  /** Uses normalizeAmount2MantissaAndExp  */
  protected def rippleEncodingOfFiatAmount(bd: BigDecimal): Either[RippleCodecError, RawValue] = {

    val fiatAmount: Either[OErrorRipple, BigDecimal] = validateFiatAmount(bd)

    fiatAmount.flatMap { amt: BigDecimal ⇒
      if (amt === 0) { // TODO: Review thisand add test cases

        val zero = ULong(1) << 63
        encodeULong(zero, "UInt64") // This is 8 bytes.
      } else {
        scribe.info(s"Encoding BigDecimal Fiat [$bd] Scale and Precision: " + amt.scale + " " + amt.precision)

        val negative: Boolean = amt.signum == -1

        // 16 bits, only bottom two used
        val signBits: ULong = if (negative) ULong(2) else ULong(3)
        val top2Bits        = signBits << 62

        // The top 3 nibbles are screwed
        val res: Either[RippleCodecError, ULong] = for {
          normalized     ← calcNormalizedMantissaAndExp(amt)
          _              = scribe.info(s"Normalized $normalized")
          (nman, nexp)   = normalized
          expBitsShifted = ULong(nexp + 97L) << 54
          top10          = top2Bits | expBitsShifted
          full           = top10 | nman
        } yield full

        val asBytes: Either[RippleCodecError, RawValue] = res.flatMap(encodeULong(_, "UInt64"))
        asBytes
      }
    }
  }

  /** V2: Given normalized amount encode it in ripple binary format */
  protected def binaryFormatFiatAmount(isNegative: Boolean,
                                       mantissa: ULong,
                                       exp: Int): Either[RippleCodecError, RawValue] = {

    scribe.info(s"Encoding BigDecimal Fiat $isNegative  $mantissa  ^  $exp")

    if (mantissa === ULong(0) && exp === 0) {
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

  /**
    * @param mantissa
    * @param exponent
    *
    * @return tuple, normalized mantissa, exponent  - if underflow both fields set to 0
    *         Overflow returned as Error
    */
  protected def rippleNormalizeScale(mantissa: ULong, exponent: Int): Either[RippleCodecError, (ULong, Int)] = {
    // Okay, we have a mantissa and and exp. We want (generally) to increate the mantissa until 10000000L which means
    // the exponent has to decrease
    // A quick literal translation

    // Before we do this we have converted mantissa to abs() and recorded the sign bit.
    // So the equivalent C++ code (which stores in  std::int64_t) is not needed.

    BinCodecExeption.wrap(s"Error Normalizing $mantissa ^ $exponent") {
      var mant = mantissa
      var exp  = exponent

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
      else (mant, exp).asRight

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
        RippleCodecError(s"Too Much Precision $amt was ${amt.precision} w/ Scale ${amt.scale}").asLeft

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

  /**
    * This is an inefficient hack, since this will be normalized later too.
    * But it is nice way to parse from JSON effectively.
    *
    * @param bd
    *
    * @return Absolute value of bd scales by 10^n such that the value is .xyyyyyyyy..   where x != 0
    **/
  def oldPrenormalize(bd: BigDecimal): (ULong, Int) = {

    // DB = unscaledValue * 10 ^scale   , precision is num digits in unscaledValue (left zeros ignored)

    val amount = bd.abs
    // Note this is intended to be normalized later, so 10 => 10E0 100 to 100E0

    val scale: Int = amount.scale

    val (man, exp): (ULong, Int) =
      if (scale === 0) {
        // No decimal points, so its all mantissa. Mantissa cannot be more
        // than 16 9's, so we need to overflow or hope that their are trailing zeros
        val shiftLeft = bd.precision - 16
        if (shiftLeft > 0) {
          val scaler: BigDecimal = spire.math.pow(BigDecimal(10L), shiftLeft)
          val scaled             = amount / scaler
          scribe.debug(s"Scaled by $shiftLeft is $scaled")
          val ul = ULong.fromLong(scaled.longValue)
          (ul, shiftLeft)
        } else {
          // Shift and then check if there is scale that is non zero?
          // Why is there no bd.getMantissa?
          val ul = if (bd.isValidLong) {
            ULong.fromLong(bd.longValue)
          } else {
            scribe.error("Coding Error in normalization of Fiat")
            ULong(0)
          }
          (ul, scale)
        }
      } else if (scale > 0) {
        // Scale is the number of digits to the right of decimal point
        // n = unscaledValue * 10^scale
        //bd.pow()
        // This needs to cover cases like 10000000000000000 where mantissa is bigger than ulong
        // but zeros on the right of the mantissa
        val scaler: BigDecimal = spire.math.pow(BigDecimal(10L), BigDecimal(scale.toLong)) // 10 * 75 not happy for long
        val scaled             = amount * scaler
        val ul                 = ULong.fromLong(scaled.longValue)
        scribe.debug(s"PreNormalized to $ul + ${-scale} using $scaler => $scaled")
        (ul, -scale)
      } else if (scale < 0) { // No Decimal places
        // n = unscaledValue * 10^ (-scale)
        val shiftLeft: Int     = -(bd.precision.toInt - 1)
        val scaler: BigDecimal = spire.math.pow(BigDecimal(10L), shiftLeft)

        val scaled = amount * scaler
        val ul     = ULong.fromLong(scaled.longValue())
        scribe.debug(s"PreNormalized to $ul + ${bd.precision} using $scaler => $scaled")
        (ul, shiftLeft)
      } else (ULong(0), 0)

    scribe.debug(s"Amount $bd Precision ${bd.precision} Scale ${bd.scale}  Man $man Exp = $exp ")
    (man, exp) // Maybe scale - 1  to get back to x.yyyyyy ^ scale = number
  }

}

object FiatAmountCodec extends FiatAmountCodec
