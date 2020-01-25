package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._

import com.odenzo.ripple.bincodec.{BCJsonErr, BCLibErr, BinCodecLibError}
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.scodec.spire._
import cats.implicits._
import io.circe.{Decoder, Json}
import scodec.bits.{BitVector, ByteVector, _}
import scodec._
import scodec.codecs._
import scodec.codecs.implicits._
import spire.math.ULong

/**
  * https://xrpl.org/currency-formats.html#issued-currency-math
  * ULong max is 18,446,744,073,709,551,615
  * First bit is taken to indicate its not XRP.
  *
  *  54-bit mantissa normalized to (10^15 ,10^16-1)
  *  8-bit exponent which is math exponent encoded +97 (uint)
  */
trait AmountScodecs extends CodecUtils {

  final val maxXrp: Double = Math.pow(10, 17)

  def xprAmountEncFn(xrp: Long) = {
    if (xrp < 0) Attempt.failure(Err(s"$xrp < 0"))
    else if (xrp > maxXrp) Attempt.failure(Err(s"$xrp < 0"))
    else ulong(62).encode(xrp).map(bin"01" ++ _)
  }

  val xrpAmountDec: Codec[Long] = constant(bin"01") dropLeft ulong(62)

  def xprAmountDecFn(bitv: BitVector) = xrpAmountDec.decode(bitv)

  // We also have the Ripple alphabet to verify the ASCII pseudo-ISO (ISO4127)
  // Don't forget the legacy currency code which is removed from current XRPL docs
  // This sure looks like a peek() or flatZip case
  def currencycodeLegacy = constant(hex"01") ~> bitsStrict(152)

  def currencycode: Codec[String] =
    constant(hex"00") ~> constantLenient(bin"0".padTo(88)) ~> fixedSizeBits(24, ascii) <~ constantLenient(bin"0".padTo(40))

  /**
    * Used to check if ISO currency codes are ok.
    *
    * @param s
    *
    * @return true is valid
    */
  protected def isRippleAscii(s: String): Boolean = {
    import com.odenzo.ripple.bincodec.reference.RippleConstants
    s.forall(c => RippleConstants.rippleCurrencyAlphabet.contains(c))
  }

  // Leave off the account codec for now
  val fiatAmount: Codec[BigDecimal] =
    (constant(bin"1") ~> bool(1) ~ ulong(8) ~ ulong(54))
      .exmap[BigDecimal](liftF3ToNestedTupleF(unpackToBigDecimal), packToBigDecimal)

  def unpackToBigDecimal(isPositive: Boolean, exponent: Long, mantissa: Long): Attempt[BigDecimal] = {
    val answer = BigDecimal(BigInt(mantissa), exponent.toInt) // Bastard, exponent cannot be above Int Max Value s
    Attempt.successful(answer)
  }

  // Placeholder and where most of the work is
  def packToBigDecimal(bd: BigDecimal): Attempt[((Boolean, Long), Long)] = {
    val answer: ((Boolean, Long), Long) = ((true, 10L), 20023L)
    val success                         = Attempt.successful(answer)
    success
  }

}
