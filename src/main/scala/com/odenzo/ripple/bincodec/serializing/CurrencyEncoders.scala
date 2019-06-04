package com.odenzo.ripple.bincodec.serializing

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.{Json, JsonObject}
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.serializing.BinarySerializer.{NestedEncodedValues, RawEncodedValue}
import com.odenzo.ripple.bincodec.serializing.TypeSerializers.encodeULong
import com.odenzo.ripple.bincodec.utils.caterrors.{CodecError, AppException, AppJsonError, OError}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}

/**
  * Binary Encoders for XRP and IOUAmount, including currency
  * IMportant -- some objects may not have an amount ... I guess replace with amount ZERO
  */
object CurrencyEncoders extends StrictLogging with JsonUtils {

  /* The range for the mantissa when normalized */
  private val minMantissa: ULong = ULong("1000000000000000")
  private val maxMantissa: ULong = ULong("9999999999999999")

  /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
  private val minExponent: Int  = -96
  private val maxExponent: Int  = 80
  private val maxPrecision: Int = 16

  /**  0x8000000000000000000000000000000000000000 to represent special case encoding of 0 fiat amoutn */
  private val zeroFiatAmount: List[UByte] = UByte(0x80) :: List.fill(19)(UByte(0))
  private val rippleAscii: String =
    "abcdefghijklmnopqrstuvwxyz" +
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "0123456789" +
      "<>(){}[]|?!@#$%^&*"

  /**
    * https://developers.ripple.com/currency-formats.html
    *
    * @param json FieldData  representing an amount, either one line XRP of object with IOU/Fiat
    */
  def encodeAmount(json: Json): Either[CodecError, BinarySerializer.Encoded] = {
    json.asObject match {
      case None      ⇒ encodeXrpAmount(json)
      case Some(obj) ⇒ encodeIOU(obj)
    }
  }

  /** In Progress, UInt64 encoding **/
  def encodeXrpAmount(v: Json): Either[CodecError, RawEncodedValue] = {

    val mask: ULong    = ULong.fromLong(0x4000000000000000L)
    val maxXRP: BigInt = spire.math.pow(BigInt(10), BigInt(17))

    val value: Either[CodecError, BigInt] = json2string(v).map(t ⇒ BigInt(t))

    val answer: Either[AppJsonError, RawEncodedValue] = value match {
      case Left(err)                ⇒ AppJsonError("Could not decode as BigInt", v).asLeft
      case Right(bi) if bi < 0      ⇒ AppJsonError(s"XRP Cant Be <0  $bi", v).asLeft
      case Right(bi) if bi > maxXRP ⇒ AppJsonError(s"XRP > $maxXRP  $bi", v).asLeft
      case Right(bi)                ⇒ encodeULong(ULong(bi.toLong) | mask, "UInt64")
    }
    answer
  }

  /** This actually returns an Amount, need to look at that encoding.
    * Probably this should be NestedAncodedAmount at the least. */
  def encodeIOU(v: JsonObject): Either[CodecError, NestedEncodedValues] = {
    // currency , value and issuer
    // 384 bits (64 + 160 + 160)     (currency, ?, ?)
    // 10 (8bit mantisa) 54 bit mantissa, 160 bit currency code, 160 bit account
    // If the amount is zero a special amount if returned...

    val attempt: Either[CodecError, NestedEncodedValues] = for {
      value    ← findField("value", v).flatMap(encodeFiatValue)
      currency ← findField("currency", v).flatMap(j ⇒ encodeCurrency(j))
      issuer   ← findField("issuer", v).flatMap(AccountIdCodecs.encodeAccountNoVL)
    } yield NestedEncodedValues(List(value, currency, issuer))

    attempt
  }

  /**
    *  Encodes non-XRP currency.
    *  Currency must be three ASCII characters. could pad left if short I guess
    *  Note that "00000000000..." is used for currency XRP in some places.
    **/
  def encodeCurrency(json: Json): Either[CodecError, RawEncodedValue] = {
    // This should alwqays be 160 bits long

    val bit90Zero: List[UByte] = List.fill(12)(UByte(0))
    val bit40Zero: List[UByte] = List.fill(5)(UByte(0))

    val iso: Either[CodecError, List[UByte]] = Either
                                               .fromOption(json.asString, CodecError("Currency Not a String"))
                                               .flatMap { s ⇒
        if (isRippleAscii(s) && s.length == 3) {
          s.getBytes("UTF-8").map(UByte(_)).toList.asRight
        } else if (s.length == 40) {
          // It should be a 160 bit hex string
          ByteUtils.hex2ubytes(s)
          // "0158415500000000C1F76FF6ECB0BAC600000000"
        } else {
          CodecError(s"Currency Code $s not three ascii").asLeft
        }
      }

    val ok = iso.map(c ⇒ bit90Zero ::: c ::: bit40Zero)
    ok.fmap(RawEncodedValue)
  }

  /**
    * Encode the amount field of a fiat value structure.
    * Fiat value can be positive or negative and very large or very
    * small (e.g.
    * 0.0000004)
    * Adapted from docs and IOUAmount.cpp in rippled code.
    * NOTE: THIS IS BADLY BROKEN
    *
    * @param json
    */
  def encodeFiatValue(json: Json): Either[CodecError, RawEncodedValue] = {
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
      if (scale == 0) {
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

  def validateFiatAmount(amount: BigDecimal): Either[OError, BigDecimal] = {
    val minVal: BigDecimal      = BigDecimal("-9999999999999999E80") ///?!?
    val maxVal: BigDecimal      = BigDecimal("9999999999999999e80")
    val maxPrecision: Int       = 15
    val maxMantissa: BigDecimal = BigDecimal("9999999999999999")
    val maxLong: BigDecimal     = BigDecimal(Long.MaxValue)

    if (amount < minVal) {
      logger.info(s"$amount less than min - underflow to ZERO")
      BigDecimal(0).asRight
    } else if (amount > maxVal) {
      CodecError(s"Overflow FiatAmount $amount < $maxVal").asLeft

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

  def normalizeAmount2MantissaAndExp(bd: BigDecimal): Either[CodecError, (ULong, Int)] = {
    /* Trouble with this,
     *
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
    val normalized: Either[CodecError, (ULong, Int)] = properNormalize(amount, exp)
    logger.info("Ripple Normalized: " + normalized)
    normalized
  }

  def rippleEncodingOfFiatAmount(bd: BigDecimal): Either[CodecError, RawEncodedValue] = {

    /*
    Minimum nonzero absolute value: 1000000000000000e-96
    Maximum value: 9999999999999999e80
    Minimum value: -9999999999999999e80
    15 decimal digits of precision

     */
    validateFiatAmount(bd).flatMap { amt: BigDecimal ⇒
      if (amt == 0) {
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
        val res: Either[CodecError, ULong] = for {
          normalized     ← normalizeAmount2MantissaAndExp(amt)
          _              = logger.info(s"Normalized $normalized")
          (nman, nexp)   = normalized
          expBitsShifted = ULong(nexp + 97L) << 54
          top10          = top2Bits | expBitsShifted
          full           = top10 | nman
        } yield full

        val asBytes: Either[CodecError, RawEncodedValue] = res.flatMap(encodeULong(_, "UInt64"))
        asBytes
      }
    }
  }

  /**
    * Used to check if ISO currency codes are ok.
    *
    * @param s
    *
    * @return  true is valid
    */
  def isRippleAscii(s: String): Boolean = { s.forall(c ⇒ rippleAscii.contains(c)) }

  /*

void
IOUAmount::normalize ()
{
    if (mantissa_ == 0)
    {
   *this = beast::zero;
        return;
    }

    bool const negative = (mantissa_ < 0);

    if (negative)
        mantissa_ = -mantissa_;

    while ((mantissa_ < minMantissa) && (exponent_ > minExponent))
    {
        mantissa_ *= 10;
        --exponent_;
    }

    while (mantissa_ > maxMantissa)
    {
        if (exponent_ >= maxExponent)
            Throw<std::overflow_error> ("IOUAmount::normalize");

        mantissa_ /= 10;
        ++exponent_;
    }

    if ((exponent_ < minExponent) || (mantissa_ < minMantissa))
    {
   *this = beast::zero;
        return;
    }

    if (exponent_ > maxExponent)
        Throw<std::overflow_error> ("value overflow");

    if (negative)
        mantissa_ = -mantissa_;
}
   */
  /**
    * @param mantissa
    * @param exponent
    * @return tuple, normalized mantissa, exponent  - if underflow both fields set to 0
    *         Overflow returned as Error
    */
  def properNormalize(mantissa: ULong, exponent: Int): Either[CodecError, (ULong, Int)] = {
    // Okay, we have a mantissa and and exp. We want (generally) to increate the mantissa until 10000000L which means
    // the exponent has to decrease
    // A quick literal translation

    // Before we do this we have converted mantissa to abs() and recorded the sign bit.
    // So the equivalent C++ code (which stores in  std::int64_t) is not needed.
    // Do this because easier, and we do know math or anything that depends on sign.
    // May want to make an STAmount later, but not now.

    //val negative = mantissa.signed < 0L  // Not sure I understand this. Can an amount be negative? ull
    //var mant =  if (negative) ULong(-mantissa.signed) else mantissa
    AppException.wrap(s"Error Normalizing $mantissa ^ $exponent") {
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

  def checkMantissaBounds(mantissa: ULong): Either[OError, ULong] = {
    /* The range for the mantissa when normalized */
    val minMantissa: ULong = ULong("1000000000000000")
    val maxMantissa: ULong = ULong("9999999999999999")
    if (mantissa < minMantissa || mantissa > maxMantissa) {
      CodecError(s"$mantissa has to be in range $minMantissa - $maxMantissa").asLeft
    } else {
      mantissa.asRight
    }

  }

  def checkExponentBounds(exp: Int): Either[OError, Int] = {
    /* The range for the exponent when normalized */
    val minExponent: Int = -96
    val maxExponent: Int = 80
    if (exp < minExponent || exp > maxExponent) {
      CodecError(s"$exp has to be in range $minExponent to $maxExponent inclusive ").asLeft
    } else {
      exp.asRight
    }

  }

}
