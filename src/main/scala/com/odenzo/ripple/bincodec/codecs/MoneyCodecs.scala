package com.odenzo.ripple.bincodec.codecs

import scala.collection.immutable

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.{Json, JsonObject}
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.reference.FieldInfo
import com.odenzo.ripple.bincodec.utils.caterrors.{AppJsonError, BinCodecExeption, OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{DecodedField, Encoded, EncodedNestedVals, RawValue}

/**
  * Binary Encoders for XRP and IOUAmount, including currency
  * IMportant -- some objects may not have an amount ... I guess replace with amount ZERO
  * This needs some cleanup for sure.
  */
trait MoneyCodecs extends StrictLogging with CodecUtils with JsonUtils {

  /* The range for the mantissa when normalized */
  private val minMantissa: ULong = ULong("1000000000000000")
  private val maxMantissa: ULong = ULong("9999999999999999")

  /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
  private val minExponent: Int = -96
  private val maxExponent: Int = 80

  private val maxXRP: BigInt = spire.math.pow(BigInt(10), BigInt(17))

  /**  0x8000000000000000000000000000000000000000 to represent special case encoding of 0 fiat amount xrp */
  private val zeroFiatAmount: List[UByte] = UByte(0x80) :: List.fill(19)(UByte(0))

  /** Valid currency characters */
  private val rippleAscii: String =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "0123456789" +
      "<>(){}[]|?!@#$%^&*"

  /**
    * https://developers.ripple.com/currency-formats.html
    *
    * @param json FieldData  representing an amount, either one line XRP of object with IOU/Fiat
    */
  def encodeAmount(json: Json): Either[RippleCodecError, Encoded] = {
    json.asObject match {
      case None      ⇒ encodeXrpAmount(json)
      case Some(obj) ⇒ encodeIOU(obj)
    }
  }

  def decodeAmount(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (DecodedField, List[UByte])] = {
    val TOP_BIT_MASK: UByte = UByte(128)
    if ((v.head & TOP_BIT_MASK) === UByte(0)) { //XRP
      decodeToUBytes(8, v, info)
    } else { // Fiat
      decodeToUBytes(48, v, info)
    }
  }

  /** In Progress, UInt64 encoding **/
  def encodeXrpAmount(v: Json): Either[RippleCodecError, RawValue] = {

    val mask: ULong = ULong.fromLong(0x4000000000000000L)

    json2string(v).map(t ⇒ BigInt(t)) match {
      case Left(err)                ⇒ AppJsonError("Could not decode as Xrp Amount BigInt", v).asLeft
      case Right(bi) if bi < 0      ⇒ AppJsonError(s"XRP Cant Be <0  $bi", v).asLeft
      case Right(bi) if bi > maxXRP ⇒ AppJsonError(s"XRP > $maxXRP  $bi", v).asLeft
      case Right(bi)                ⇒ encodeULong(ULong(bi.toLong) | mask, "UInt64")
    }

  }

  def encodeIOU(v: JsonObject): Either[RippleCodecError, EncodedNestedVals] = {
    // currency , value and issuer
    // 384 bits (64 + 160 + 160)     (currency, ?, ?)
    // 10 (8bit mantisa) 54 bit mantissa, 160 bit currency code, 160 bit account
    // If the amount is zero a special amount if returned... TODO: Check if correct

    for {
      value    ← findField("value", v).flatMap(encodeFiatValue)
      currency ← findField("currency", v).flatMap(encodeCurrency)
      issuer   ← findField("issuer", v).flatMap(AccountIdCodecs.encodeAccountNoVL)
    } yield EncodedNestedVals(List(value, currency, issuer))
  }

  /**
    *  Encodes non-XRP currency.
    *  Currency must be three ASCII characters. could pad left if short I guess
    *  Note that "00000000000..." is used for currency XRP in some places.
    *  TODO: Non ASCII currency (pre-hex encoded) is not tested or validated  yet
    **/
  def encodeCurrency(json: Json): Either[RippleCodecError, RawValue] = {
    // This should alwqays be 20 bytes long

    val bit90Zero: List[UByte] = List.fill(12)(UByte(0))
    val bit40Zero: List[UByte] = List.fill(5)(UByte(0))
    //  It should be a 160 bit hex string but can't be XRP
    // "0158415500000000C1F76FF6ECB0BAC600000000"

    json2string(json)
      .flatMap {
        case s if isRippleAscii(s) && s.length === 3 ⇒
          val curr = ByteUtils.bytes2ubytes(s.getBytes("UTF-8")).toList
          RawValue(bit90Zero ::: curr ::: bit40Zero).asRight

        case s if s.length === 40 ⇒ ByteUtils.hex2ubytes(s).map(RawValue)

        case other ⇒ RippleCodecError(s"Currency $other not three ascii").asLeft
      }

  }

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
    json2string(json)
      .map(BigDecimal(_))
      .flatMap(rippleEncodingOfFiatAmount)

  }

  /**
    *  This is an inefficient hack, since this will be normalized later too.
    *  But it is nice way to parse from JSON effectively.
    * @param bd
    *
    * @return Absolute value of bd scales by 10^n such that the value is .xyyyyyyyy..   where x != 0
    **/
  def normalizeToIntegral(bd: BigDecimal): (ULong, Int) = {

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
          logger.debug(s"Scaled by $shiftLeft is $scaled")
          val ul = ULong.fromLong(scaled.longValue)
          (ul, shiftLeft)
        } else {
          // Shift and then check if there is scale that is non zero?
          // Why is there no bd.getMantissa?
          val ul = if (bd.isValidLong) {
            ULong.fromLong(bd.longValue)
          } else {
            logger.error("Coding Error in normalization of Fiat")
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
        logger.debug(s"PreNormalized to $ul + ${-scale} using $scaler => $scaled")
        (ul, -scale)
      } else if (scale < 0) { // No Decimal places
        // n = unscaledValue * 10^ (-scale)
        val shiftLeft: Int     = -(bd.precision.toInt - 1)
        val scaler: BigDecimal = spire.math.pow(BigDecimal(10L), shiftLeft)

        val scaled = amount * scaler
        val ul     = ULong.fromLong(scaled.longValue())
        logger.debug(s"PreNormalized to $ul + ${bd.precision} using $scaler => $scaled")
        (ul, shiftLeft)
      } else (ULong(0), 0)

    logger.debug(s"Amount $bd Precision ${bd.precision} Scale ${bd.scale}  Man $man Exp = $exp ")
    (man, exp) // Maybe scale - 1  to get back to x.yyyyyy ^ scale = number
  }

  def validateFiatAmount(amount: BigDecimal): Either[OErrorRipple, BigDecimal] = {
    val minVal: BigDecimal = BigDecimal("-9999999999999999E80") ///?!?
    val maxVal: BigDecimal = BigDecimal("9999999999999999e80")
    val maxPrecision: Int  = 15

    if (amount < minVal) {
      logger.info(s"$amount less than min - underflow to ZERO")
      BigDecimal(0).asRight
    } else if (amount > maxVal) {
      RippleCodecError(s"Overflow FiatAmount $amount < $maxVal").asLeft

    } else if (amount.precision > maxPrecision) {
      // Too much precision, some will be ignored. But if close to zero make zero?
      // FIXME: Max Precision check hacked ... add cases to round down to ZERO on underflow
      //AppError(s"Prevision Overflow $amount ${amount.precision} > $maxPrecision").asLeft

      // Well, what to do here.
      logger.debug(s"Too Much Precision $amount was ${amount.precision} w/ Scale ${amount.scale}")
      amount.asRight
    } else {
      amount.asRight
    }

  }

  def normalizeAmount2MantissaAndExp(bd: BigDecimal): Either[RippleCodecError, (ULong, Int)] = {
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
    val (amount: ULong, exp: Int) = normalizeToIntegral(bd.abs)
    logger.info(s"Pre Normalized Mantissa $amount  Exponent $exp ")
    val normalized: Either[RippleCodecError, (ULong, Int)] = properNormalize(amount, exp)
    logger.info("Ripple Normalized: " + normalized)
    normalized
  }

  def rippleEncodingOfFiatAmount(bd: BigDecimal): Either[RippleCodecError, RawValue] = {

    /*
    Minimum nonzero absolute value: 1000000000000000e-96
    Maximum value: 9999999999999999e80
    Minimum value: -9999999999999999e80
    15 decimal digits of precision

     */
    validateFiatAmount(bd).flatMap { amt: BigDecimal ⇒
      if (amt == 0) { // TODO: Review thisand add test cases
        // 0x8000000000000000000000000000000000000000 say doc as 64 bit, which makes no sense
        val zero = ULong(1) << 63
        encodeULong(zero, "UInt64") // This is 8 bytes.
      } else {
        logger.info(s"Encoding BigDecimal Fiat [$bd] Scale and Precision: " + amt.scale + " " + amt.precision)

        val negative: Boolean = amt.signum == -1

        // 16 bits, only bottom two used
        val signBits: ULong = if (negative) ULong(2) else ULong(3)
        val top2Bits        = signBits << 62

        // The top 3 nibbles are screwed
        val res: Either[RippleCodecError, ULong] = for {
          normalized     ← normalizeAmount2MantissaAndExp(amt)
          _              = logger.info(s"Normalized $normalized")
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

  /**
    * @param mantissa
    * @param exponent
    * @return tuple, normalized mantissa, exponent  - if underflow both fields set to 0
    *         Overflow returned as Error
    */
  def properNormalize(mantissa: ULong, exponent: Int): Either[RippleCodecError, (ULong, Int)] = {
    // Okay, we have a mantissa and and exp. We want (generally) to increate the mantissa until 10000000L which means
    // the exponent has to decrease
    // A quick literal translation

    // Before we do this we have converted mantissa to abs() and recorded the sign bit.
    // So the equivalent C++ code (which stores in  std::int64_t) is not needed.
    // Do this because easier, and we do know math or anything that depends on sign.
    // May want to make an STAmount later, but not now.

    //val negative = mantissa.signed < 0L  // Not sure I understand this. Can an amount be negative? ull
    //var mant =  if (negative) ULong(-mantissa.signed) else mantissa
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

  /**
    * Used to check if ISO currency codes are ok.
    *
    * @param s
    *
    * @return true is valid
    */
  protected def isRippleAscii(s: String): Boolean = { s.forall(c ⇒ rippleAscii.contains(c)) }

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

object MoneyCodecs extends MoneyCodecs
