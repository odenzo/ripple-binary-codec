package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.ULong

import com.odenzo.ripple.bincodec.{BCLibErr, RawValue, BinCodecLibError}
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.utils.JsonUtils

/**
  * https://xrpl.org/currency-formats.html#issued-currency-math
  * ULong max is 18,446,744,073,709,551,615
  * First bit is taken to indicate its not XRP.
  *
  *  54-bit mantissa normalized to (10^15 ,10^16-1)
  *  8-bit exponent which is math exponent encoded +97 (uint)
  */
trait IssuedAmountCodec extends CodecUtils {

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
    BinCodecLibError.handlingM(s"Error Normalizing Fiat Amount ${json.spaces4}") {
      val doLatest = true
      for {
        str <- JsonUtils.json2string(json)
        amt = BigDecimal(str)
        enc <- normalize(amt)

      } yield enc
    }
  }

  def normalize(amount: BigDecimal): Either[BinCodecLibError, RawValue] = {
    amount match {
      case a if a === 0              => UIntCodecs.encodeULong(ULong(1) << 63, "UInt64") // This is 8 bytes.
      case a if a < minVal           => BinCodecLibError(s"amount too small $a").asLeft
      case a if a > maxVal           => BinCodecLibError(s"amount too big $a").asLeft
      case a if a.abs < minAbsAmount => BinCodecLibError(s"amount too close to zero $a").asLeft
      case amt                       =>
        // Okay, now lets get a mantissa that fits in a ULong, or better in 10^15 .. 10^16-1 to fit in 54-bits
        val shiftPlaces: Int       = 16 + (amt.scale - amt.precision)
        val normalized: BigDecimal = amt * BigDecimal.exact(10).pow(shiftPlaces)
        val trueExp                = -shiftPlaces
        scribe.info(s"NEW Exponent/Mant:$amt ${amt.scale} ${amt.precision} =>  $trueExp / $normalized")
        // We may still have a value with too much precision if after normalizaing there are some items left of decimal
        // This is where I am not sure the defined behavoir of rippled, truncate would be ok I guess
        normalized.isWhole match {
          case false => BinCodecLibError(s"Unsure how to handle too much precision so error ${amount}").asLeft
          case true  => encodeToBinary(normalized, trueExp)
        }
    }
  }

  protected def encodeToBinary(amtNormalized: BigDecimal, exp: Int): Either[BinCodecLibError, RawValue] = {

    val negativeBits = ULong(2) << 62
    val positiveBits = ULong(3) << 62

    scribe.debug(s"Encoding BigDecimal Fiat $amtNormalized  True Exp: $exp")

    val mantissa              = ULong(amtNormalized.abs.longValue)
    val signBits: ULong       = if (amtNormalized.signum == -1) negativeBits else positiveBits // 2 for negative
    val expBitsShifted: ULong = ULong(exp + 97L) << 54
    val top10: ULong          = signBits | expBitsShifted
    val full: ULong           = top10 | mantissa

    val asBytes: Either[BinCodecLibError, RawValue] = UIntCodecs.encodeULong(full, "UInt64")
    asBytes

  }
}

object IssuedAmountCodec extends IssuedAmountCodec
