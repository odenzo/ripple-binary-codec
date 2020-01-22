package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.{BCJsonErr, BCLibErr, BinCodecLibError}
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import scodec.bits._
import scodec.codecs._
import cats.implicits._
import io.circe.{Decoder, Json}
import scodec.bits.Bases.Alphabets
import scodec.bits.{BitVector, ByteVector}
import scodec._
import scodec.codecs._
import spire.math.UInt

import com.odenzo.ripple.bincodec.codecs.MoneyCodecs

/**
  * https://xrpl.org/currency-formats.html#issued-currency-math
  * ULong max is 18,446,744,073,709,551,615
  * First bit is taken to indicate its not XRP.
  *
  *  54-bit mantissa normalized to (10^15 ,10^16-1)
  *  8-bit exponent which is math exponent encoded +97 (uint)
  */
trait AmountScodecs extends CodecUtils {

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

  /** This is closest number to zero that is valid, ie smallest absolute value  */
  /**
    * Encode the amount field of a fiat value structure.
    * Fiat value can be positive or negative and very large or very
    * small (e.g.
    * 0.0000004)
    *

    */
  def xrpFiatEnc(issuedAmt: String): Attempt[BitVector] = {
    BinCodecLibError
      .handlingM(s"Error Normalizing Fiat Amount ${issuedAmt}") {
        normalize(BigDecimal(issuedAmt))
      }
      .fold(e => Attempt.failure[ByteVector](Err(e.msg)), Attempt.successful)
      .map(_.bits)
  }

  /** Encoder Only as this point */
  val xrpfiat: Encoder[String] = scodec.Encoder(xrpFiatEnc _)

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

  /** Mask to set the top bits of an XRP Amount */
  private val mask: ByteVector = hex"0x4000000000000000"

  /**
    * https://developers.ripple.com/currency-formats.html
    *
    * @param json FieldData  representing an amount, either one line XRP of object with IOU/Fiat
    */
  def encodeAmount(json: Json): Either[BinCodecLibError, ByteVector] = {
    json.asObject match {
      case None      => encodeXrpAmount(json)
      case Some(obj) => encodeIOU(json)
    }
  }

  //  /**
  //    *
  //    * @return XRP Hex with the 63th bit set to 1 (not checked) or 48 bytes of fiat amount
  //    */
  //  def decodeAmount(v: List[UByte], info: FieldMetaData): Either[BCLibErr, (DecodedField, List[UByte])] = {
  //
  //    val TOP_BIT_MASK: UByte = UByte(128)
  //    val SIGN_BIT_MASK       = ~UByte(64) // Sign Bit is always 1 for XRP
  //
  //    v match {
  //      case h :: t if (h & TOP_BIT_MASK) === UByte(0) => // Note sure why I have to set sign bit, should be set
  //        CodecUtils.decodeToUBytes(8, (h | SIGN_BIT_MASK) :: t, info) // XRP
  //      case other => CodecUtils.decodeToUBytes(48, other, info) // Fiat
  //    }
  //
  //  }

  /** This is expressed as a string json field representing number of drops **/
  def encodeXrpAmount(v: Json): Either[BinCodecLibError, ByteVector] = {
    import com.odenzo.ripple.bincodec.reference.RippleConstants.maxDrops
    import com.odenzo.ripple.bincodec.reference.RippleConstants
    decode(v, Decoder.decodeBigInt).flatMap {
      case bi if bi < 0        => BCJsonErr(s"XRP Cant Be <0  $bi", v).asLeft
      case bi if bi > maxDrops => BCJsonErr(s"XRP > $maxDrops  $bi", v).asLeft
      case bi                  => (ulong(60).encode(bi.toLong).require.padLeft(64).bytes | mask).asRight
    }

  }

  /** Encode IOU / Issued Amount , in this case account has VL encoding */
  def encodeIOU(v: Json): Either[BinCodecLibError, ByteVector] = {
    // currency , value and issuer
    // 384 bits (64 + 160 + 160)     (currency, ?, ?)
    // 10 (8bit mantisa) 54 bit mantissa, 160 bit currency code, 160 bit account
    // If the amount is zero a special amount if returned... TODO: Check if correct
    import com.odenzo.ripple.bincodec.reference.RippleConstants
    for {
      amountField <- findField("value", v)
      amount      <- decode(amountField, Decoder.decodeBigDecimal, "Decoding Fiat Value".some)
      full <- if (amount.compareTo(BigDecimal(0)) === 0) {
        RippleConstants.rawEncodedZeroFiatAmount.asRight
      } else {
        encodeFullIOU(v)
      }
    } yield full
  }

  protected def encodeFullIOU(jobj: Json): Either[BinCodecLibError, ByteVector] = {
    val json = jobj.dropNullValues

    def encodeField(name: String, fn: String => Either[BinCodecLibError, ByteVector]): Either[BinCodecLibError, ByteVector] = {
      findField(name, json).flatMap(json2string).flatMap(fn)
    }

    for {
      currency <- encodeField("currency", MoneyCodecs.encodeCurrency)
      value    <- encodeField("value", IssuedAmountCodec.encodeFiatValue)
      issuer   <- encodeField("issuer", AccountIdCodecs.encodeAccountNoVL)
    } yield value ++ currency ++ issuer

  }

  /**
    * Encodes non-XRP currency.
    * Currency must be three ASCII characters. could pad left if short I guess
    * Note that "00000000000..." is used for currency XRP in some places.
    * TODO: Non ASCII currency (pre-hex encoded) is not tested or validated  yet
    *
    * @param currency This is expected to be the  String corresponding to just currency field
    *
    * @return 160 bits per   https://xrpl.org/currency-formats.html
   **/
  def encodeCurrency(currency: String): Either[BinCodecLibError, ByteVector] = {
    val bit90Zero: ByteVector  = hex"00".padTo(12)
    val bit40Zero: ByteVector  = hex"00".padTo(5)
    val bit160Zero: ByteVector = hex"00".padTo(20)

    currency match {
      case "XRP"                                      => bit160Zero.asRight
      case s if s.length === 20 && s.startsWith("00") => encodeHex(s)
      case s if s.length === 20 && s.startsWith("01") => encodeHex(s)
      case s if s.length === 3 && isRippleAscii(s)    => (bit90Zero ++ utf8.encode(s).require.bytes ++ bit40Zero).asRight
      case other                                      => BinCodecLibError(s"Invalid Currency $other").asLeft
    }

  }

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

}
