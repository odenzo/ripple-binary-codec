package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._

import com.odenzo.ripple.bincodec.{BCLibErr, BinCodecLibError}
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import cats.implicits._
import scodec.bits.{BitVector, ByteVector, _}
import scodec._
import scodec.codecs._

/**
  * https://xrpl.org/currency-formats.html#issued-currency-math
  * ULong max is 18,446,744,073,709,551,615
  * First bit is taken to indicate its not XRP.
  *
  *  54-bit mantissa normalized to (10^15 ,10^16-1)
  *  8-bit exponent which is math exponent encoded +97 (uint)
  */
trait FiatAmountScodecs extends CodecUtils {

  protected val minVal: BigDecimal       = BigDecimal("-9999999999999999E80")
  protected val maxVal: BigDecimal       = BigDecimal("9999999999999999E80")
  protected val minAbsAmount: BigDecimal = BigDecimal("1000000000000000E-96")

  protected val maxPrecision: Int = 15

  /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
  protected val minExponent: Int    = -96
  protected val maxExponent: Int    = 80
  protected val minMantissa: BigInt = BigDecimal("1e15").toBigInt // For normalizing not input
  protected val maxMantissa: BigInt = BigDecimal("10e16").toBigInt - 1 // For normalizing not input

  // 64 bits!=  20 * 8  160 bits which  doesn't match 2*160 or 3*160
  val ZERO_SPECIAL_CASE = hex"0x8000000000000000000000000000000000000000"

  def normalize(amount: BigDecimal): Either[BCLibErr, ByteVector] = {
    amount match {
      case a if a === 0              => (bin"1" << 63).bytes.asRight
      case a if a < minVal           => BinCodecLibError(s"amount too small $a").asLeft
      case a if a > maxVal           => BinCodecLibError(s"amount too big $a").asLeft
      case a if a.abs < minAbsAmount => BinCodecLibError(s"amount too close to zero $a").asLeft
      case amt                       =>
        // Okay, now lets get a mantissa that fits in a ULong, or better in 10^15 .. 10^16-1 to fit in 54-bits
        val shiftPlaces: Int       = 16 + (amt.scale - amt.precision)
        val normalized: BigDecimal = amt * BigDecimal.exact(10).pow(shiftPlaces)
        val trueExp                = -shiftPlaces

        normalized.isWhole match {
          case false => BinCodecLibError(s"Unsure how to handle too much precision so error ${amount}").asLeft
          case true  => normalized2bytes(normalized, trueExp).asRight
        }
    }
  }

  /** Encodes to 8 bytes */
  protected def normalized2bytes(amtNormalized: BigDecimal, exp: Int): ByteVector = {

    val negativeBits = bin"10" << 62
    val positiveBits = bin"11" << 62

    scribe.debug(s"Encoding BigDecimal Fiat $amtNormalized  True Exp: $exp")

    val mantissa       = ulong(63).encode(amtNormalized.abs.longValue).require.padLeft(64)
    val signBits       = if (amtNormalized.signum == -1) negativeBits else positiveBits // 2 for negative
    val expBitsShifted = BitVector.fromLong(exp + 97L) << 54
    val top10          = signBits | expBitsShifted
    val full           = top10 | mantissa

    full.bytes

  }

  //  /** Encode IOU / Issued Amount , in this case account has VL encoding */
//  def encodeIOU(v: Json): Either[BinCodecLibError, ByteVector] = {
//    // currency , value and issuer
//    // 384 bits (64 + 160 + 160)     (currency, ?, ?)
//    // 10 (8bit mantisa) 54 bit mantissa, 160 bit currency code, 160 bit account
//    // If the amount is zero a special amount if returned... TODO: Check if correct
//    import com.odenzo.ripple.bincodec.reference.RippleConstants
//    for {
//      amountField <- findField("value", v)
//      amount      <- decode(amountField, Decoder.decodeBigDecimal, "Decoding Fiat Value".some)
//      full <- if (amount.compareTo(BigDecimal(0)) === 0) {
//        RippleConstants.rawEncodedZeroFiatAmount.asRight
//      } else {
//        encodeFullIOU(v)
//      }
//    } yield full
//  }
}
