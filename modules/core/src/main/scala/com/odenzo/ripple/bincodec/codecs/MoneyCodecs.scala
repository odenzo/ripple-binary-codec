package com.odenzo.ripple.bincodec.codecs

import cats.implicits._
import io.circe.{Json, JsonObject, Decoder}
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.reference.{FieldData, FieldMetaData}
import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}

/**
  * Binary Encoders for XRP and IOUAmount, including currency
  * Some objects may not have an amount ...
  * This needs some cleanup for sure.
  *  TODO; Deal with Hash160 for currency
  */
trait MoneyCodecs extends JsonUtils {

  import scodec.Codec
  import scodec.bits._
  import scodec.codecs._

  /** Maximum XRP amount expressed in Drops, which requires only 7 bytes so safe in a Long  */
  private val maxXrpDrops: BigInt = spire.math.pow(BigInt(10), BigInt(17))

  /** represents special case encoding of 0 fiatamount  */
  private val zeroFiatAmount: Codec[Unit] = constant(hex"0x80".padRight(20))

  /** Packaged up zeroFiatAmount  */
  private val rawEncodedZeroFiatAmount = zeroFiatAmount.encode(()).require.bytes

  private val xrpCurrencyCode: Codec[Unit] = constant(hex"00".padLeft(20))

  // The raw hex form in json can be handled, but can't be XRP
  // This should be 24 bits, 'X'.ascii , 'R'.ascii, 'P'.ascii (UTF-8 equivalent in that range)
  private val correctXrpHexCode: ByteVector = utf8.encode("XRP").require.bytes

  /**  Standard XRP encoding, like an ISO **/
  private val xrpHex: ByteVector = hex"0158415500000000C1F76FF6ECB0BAC600000000"

  /** Valid currency characters */
  private val rippleAscii: String =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "0123456789" +
      "<>(){}[]|?!@#$%^&*"

  /** Mask to set the top bits of an XRP Amount */
  private val mask: ByteVector = hex"0x4000000000000000"

  /**
    * https://developers.ripple.com/currency-formats.html
    *
    * @param json FieldData  representing an amount, either one line XRP of object with IOU/Fiat
    */
  def encodeAmount(json: Json): Any = {
    json.asObject match {
      case None      => encodeXrpAmount(json)
      case Some(obj) => encodeIOU(json)
    }
  }

  /**
    *
    * @return XRP Hex with the 63th bit set to 1 (not checked) or 48 bytes of fiat amount
    */
  def decodeAmount(v: List[UByte], info: FieldMetaData): Either[BCLibErr, (DecodedField, List[UByte])] = {

    val TOP_BIT_MASK: UByte = UByte(128)
    val SIGN_BIT_MASK       = ~UByte(64) // Sign Bit is always 1 for XRP

    v match {
      case h :: t if (h & TOP_BIT_MASK) === UByte(0) => // Note sure why I have to set sign bit, should be set
        CodecUtils.decodeToUBytes(8, (h | SIGN_BIT_MASK) :: t, info) // XRP
      case other => CodecUtils.decodeToUBytes(48, other, info) // Fiat
    }

  }

  /** This is expressed as a string json field representing number of drops **/
  def encodeXrpAmount(v: Json): Either[BinCodecLibError, ByteVector] = {

    decode(v, Decoder.decodeBigInt).flatMap {
      case bi if bi < 0           => BCJsonErr(s"XRP Cant Be <0  $bi", v).asLeft
      case bi if bi > maxXrpDrops => BCJsonErr(s"XRP > $maxXrpDrops  $bi", v).asLeft
      case bi                     => (ulong(60).encode(bi.toLong).require.padLeft(64).bytes | mask).asRight
      // TODO: Make a use a more generic UINT64 that works for positive Longs >0 and less than UInt64.max and padd to 64 bitss
    }

  }

  /** Encode IOU / Issued Amount , in this case account has VL encoding */
  def encodeIOU(v: Json): Either[BinCodecLibError, Object] = {
    // currency , value and issuer
    // 384 bits (64 + 160 + 160)     (currency, ?, ?)
    // 10 (8bit mantisa) 54 bit mantissa, 160 bit currency code, 160 bit account
    // If the amount is zero a special amount if returned... TODO: Check if correct

    for {
      amountField <- findField("value", v)
      amount      <- decode(amountField, Decoder.decodeBigDecimal, "Decoding Fiat Value".some)
      full <- if (amount.compareTo(BigDecimal(0)) === 0) {
        rawEncodedZeroFiatAmount.asRight
      } else {
        encodeFullIOU(v)
      }
    } yield full
  }

  protected def encodeFullIOU(jobj: Json): Either[BinCodecLibError, EncodedNestedValue] = {
    val json = jobj.dropNullValues
    def encodeField(name: String, fn: Json => Either[BinCodecLibError, ByteVector]): Either[BinCodecLibError, ByteVector] = {
      findField(name, json).flatMap(fn)
    }
    for {
      currency <- encodeField("currency", MoneyCodecs.encodeCurrency)
      value    <- encodeField("value", IssuedAmountCodec.encodeFiatValue)
      issuer   <- encodeField("issuer", AccountIdCodecs.encodeAccountNoVL)
    } yield EncodedNestedValue(List(value, currency, issuer))

  }

  /**
    * Encodes non-XRP currency.
    * Currency must be three ASCII characters. could pad left if short I guess
    * Note that "00000000000..." is used for currency XRP in some places.
    * TODO: Non ASCII currency (pre-hex encoded) is not tested or validated  yet
    *  @param json This is expected to be the  String corresponding to just currency field
    *              @return   160 bits per   https://xrpl.org/currency-formats.html
    **/
  def encodeCurrency(json: Json): Either[BinCodecLibError, ByteVector] = {
    val bit90Zero: ByteVector  = hex"00".padTo(12)
    val bit40Zero: ByteVector  = hex"00".padTo(5)
    val bit160Zero: ByteVector = hex"00".padTo(20)

    json2string(json).flatMap {
      case "XRP"                                      => bit160Zero.asRight
      case s if s.length === 20 && s.startsWith("00") => MiscCodecs.encodeHex(s)
      case s if s.length === 20 && s.startsWith("01") => MiscCodecs.encodeHex(s) // Legacy Demurrage
      case s if s.length === 3 && isRippleAscii(s)    => (bit90Zero ++ utf8.encode(s).require.bytes ++ bit40Zero).asRight

      case other => BinCodecLibError(s"Invalid Currency $other").asLeft
    }

  }

  /**
    * Used to check if ISO currency codes are ok.
    *
    * @param s
    *
    * @return true is valid
    */
  protected def isRippleAscii(s: String): Boolean = { s.forall(c => rippleAscii.contains(c)) }

}

object MoneyCodecs extends MoneyCodecs
