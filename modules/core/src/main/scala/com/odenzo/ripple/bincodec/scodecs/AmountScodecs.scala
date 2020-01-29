package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._
import cats.implicits._
import scodec.bits.{BitVector, _}
import scodec._
import scodec.codecs._
import spire.math.ULong

/**
  * https://xrpl.org/currency-formats.html#issued-currency-math
  * ULong max is 18,446,744,073,709,551,615
  * First bit is taken to indicate its not XRP.
  *
  *  54-bit mantissa normalized to (10^15 ,10^16-1)
  *  8-bit exponent which is math exponent encoded +97 (uint)
  */
trait AmountScodecs {

  protected val minVal: BigDecimal       = BigDecimal("-9999999999999999E80")
  protected val maxVal: BigDecimal       = BigDecimal("9999999999999999E80")
  protected val minAbsAmount: BigDecimal = BigDecimal("1000000000000000E-96")

  protected val maxPrecision: Int = 15

  /* The range for the exponent when normalized (as signed Int, +97 gives range 1 to 177 unsigned) */
  protected val minExponent: Int    = -96
  protected val maxExponent: Int    = 80
  protected val minMantissa: BigInt = BigDecimal("1e15").toBigInt // For normalizing not input
  protected val maxMantissa: BigInt = BigDecimal("10e16").toBigInt - 1 // For normalizing not input

  final val maxXrp: Double = Math.pow(10, 17)

  protected def xprAmountEncFn(xrp: Long) = {
    if (xrp < 0) Attempt.failure(Err(s"XRP Amount$xrp < 0"))
    else if (xrp > maxXrp) Attempt.failure(Err(s"XRP Amount $xrp > $maxXrp"))
    else ulong(62).encode(xrp).map(bin"01" ++ _)
  }

  protected def xprAmountDecFn(bitv: BitVector) = xrpXrpAmount.decode(bitv)

  // We have consumed the first bit already
  val xrpXrpAmount: Codec[Long] = (constant(bin"1") dropLeft ulong(62)).withContext("xrpXrpAmont")

  /* We also have the Ripple alphabet to verify the ASCII pseudo-ISO (ISO4127)
   Don't forget the legacy currency code which is removed from current XRPL docs
   This sure looks like a peek() or flatZip case
   This doesn't have to match the Ripple Alphabet
   */
  def currencycodeLegacy: Codec[BitVector] = (constantLenient(bin"0000_0001") ~> bitsStrict(152)).withContext("Legacy Currency")

  /** @todo Should be checking the string is in the ripplecurrencyalphabet */
  def currencycode: Codec[String] =
    (constantLenient(bin"0".padTo(88)) ~> fixedSizeBits(24, ascii) <~ constantLenient(bin"0".padTo(40)))
      .withContext("ISO Currnecy")

  /** Handles Legacy and ISO Currency Codes. Either 3 ASCII Chard or 160 bits of Hex  either bool(8) DOES NOT consume */
  val xrplCurrency: Codec[Either[String, BitVector]] = either(bool(8), currencycode, currencycodeLegacy).withContext("Currency Sniffer")

  /**
    * Used to check if ISO currency codes are ok.
    *
    * @param s
    *
    * @return true is valid
    */
  protected def isRippleAscii(s: String): Boolean = {
    s.forall(c => RippleConstants.rippleCurrencyAlphabet.contains(c))
  }

  /** Gets JUST the amount for a Fiat Value, not the currency or issuer */

  /** Starts after the XRP/Fiat discriminator with the Sign Bit. Sign Bit, Exponent, Mantissa   The exponent is encoded +/- 73 or
    * somethig too.
    * Fixed Size of 63 */
  val fiatAmount: Codec[BigDecimal] =
    (bool(1) ~ uint8 ~ ulong(54))
      .exmap[BigDecimal](liftF3ToNestedTupleF(unpackToBigDecimal), packToBigDecimal)
      .withContext("XRPL Fiat Amount")

  def unpackToBigDecimal(isPositive: Boolean, exponent: Int, mantissa: Long): Attempt[BigDecimal] = {
    scribe.debug(s"Unpacking to BigD $isPositive Raw Exponent $exponent Raw Mantissa: (Base ?) $mantissa")
    // TODO: Adjust exponent correctly
    val exponentAdj           = exponent - 97
    val answer: BigDecimal    = BigDecimal(mantissa) * BigDecimal.exact(10).pow(exponentAdj)
    val signedAns: BigDecimal = if (isPositive) answer else (-answer)
    scribe.debug(s"BigDecimal $signedAns")
    Attempt.successful(signedAns)
  }

  def packToBigDecimal(bd: BigDecimal): Attempt[((Boolean, Int), Long)] = {
    bd match {
      case a if a === 0              => Attempt.successful(((true, 0), 0))
      case a if a < minVal           => Attempt.failure(Err(s"fiat amount too small $a"))
      case a if a > maxVal           => Attempt.failure(Err(s"fiat amount too big $a"))
      case a if a.abs < minAbsAmount => Attempt.failure(Err(s"fiat amount too close to zero $a"))
      case amt                       =>
        // Okay, now lets get a mantissa that fits in a ULong, or better in 10^15 .. 10^16-1 to fit in 54-bits
        val shiftPlaces: Int       = 16 + (amt.scale - amt.precision)
        val normalized: BigDecimal = amt * BigDecimal.exact(10).pow(shiftPlaces)
        val trueExp                = -shiftPlaces
        val isPositive             = bd.signum =!= -1
        normalized.isWhole match {
          case false => Attempt.failure(Err(s"Unsure how to handle too much precision so error $bd"))
          case true  => Attempt.successful(((isPositive, trueExp), normalized.longValue))
        }
    }
  }

  val xrpFiat: Codec[((BigDecimal, Either[String, BitVector]), String)] =
    (fixedSizeBits(63, fiatAmount) ~ fixedSizeBits(160, xrplCurrency) ~ fixedSizeBits(160, AccountScodecs.xrpaccount))
      .withContext("XRPL Fiat ")

  /** Either XRP Long or a FiatAmount of Amount, Either[StandardCurrency,VustomerCurrency], Issuer
    * Xrp/NotXrp if 1 then Issued Currency For,at (xrpFiat) else XRP Amount Format (xrpXrpAmount)*/
  val xrplAmount: Codec[Either[Long, ((BigDecimal, Either[String, BitVector]), String)]] =
    either(bool, xrpXrpAmount, xrpFiat).withContext("XRPL Fiat or XRP Amount")
}

object AmountScodecs extends AmountScodecs
