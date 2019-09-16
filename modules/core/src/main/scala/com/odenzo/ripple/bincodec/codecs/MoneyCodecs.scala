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
trait MoneyCodecs extends CodecUtils with JsonUtils {

  /** Maximum XRP amount expressed in Drops */
  private val maxXrpDrops: BigInt = spire.math.pow(BigInt(10), BigInt(17))

  /** represent special case encoding of 0 fiatamount  */
  //val zeroFiatAmount =  0x8000000000000000000000000000000000000000
  private val zeroFiatAmount: List[UByte] = UByte(0x80) :: List.fill(19)(UByte(0))
  private val rawEncodedZeroFiatAmount    = RawValue(zeroFiatAmount)

  private val xrpCurrencyCode: List[UByte] = List.fill(20)(UByte(0))
  // The raw hex form in json can be handled, but can't be XRP
  // This should be 24 bits, 'X'.ascii , 'R'.ascii, 'P'.ascii (UTF-8 equivalent in that range)
  private val correctXrpHexCode = ByteUtils.bytes2hex("XRP".getBytes("UTF-8"))
  private val xrpHex            = "0158415500000000C1F76FF6ECB0BAC600000000" // Standard XRP encoding, like an ISO
  // (garbage)

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
  def encodeAmount(json: Json): Either[BinCodecLibError, Encoded] = {
    json.asObject match {
      case None      => encodeXrpAmount(json)
      case Some(obj) => encodeIOU(obj)
    }
  }

  /**
    *
    * @param v
    * @param info
    *
    * @return XRP Hex with the 63th bit set to 1 (not checked) or 48 bytes of fiat amount
    */
  def decodeAmount(v: List[UByte], info: FieldMetaData): Either[BCLibErr, (DecodedField, List[UByte])] = {

    val TOP_BIT_MASK: UByte = UByte(128)
    val SIGN_BIT_MASK       = ~UByte(64) // Sign Bit is always 1 for XRP

    v match {
      case h :: t if (h & TOP_BIT_MASK) === UByte(0) => decodeToUBytes(8, (h | SIGN_BIT_MASK) :: t, info) // XRP
      case other                                     => decodeToUBytes(48, other, info)                   // Fiat
    }

  }

  /** This is expressed as a string json field representing number of drops **/
  def encodeXrpAmount(v: Json): Either[BinCodecLibError, RawValue] = {

    val mask: ULong = ULong.fromLong(0x4000000000000000L)

    // Check is its with drops range
    json2string(v).map(t => BigInt(t)) match {
      case Left(err)                     => BCJsonErr("Could not decode as Xrp Amount BigInt", v).asLeft
      case Right(bi) if bi < 0           => BCJsonErr(s"XRP Cant Be <0  $bi", v).asLeft
      case Right(bi) if bi > maxXrpDrops => BCJsonErr(s"XRP > $maxXrpDrops  $bi", v).asLeft
      case Right(bi)                     => encodeULong(ULong(bi.toLong) | mask, "UInt64")
    }

  }

  def extract(metadata: FieldMetaData, obj: JsonObject): Either[BinCodecLibError, FieldData] = {
    findField(metadata.name, obj).map(json => FieldData(json, metadata))
  }

  /** Encoded IOU / Issued Amount , in this case account has VL encoding */
  def encodeIOU(v: JsonObject): Either[BinCodecLibError, Encoded] = {
    // currency , value and issuer
    // 384 bits (64 + 160 + 160)     (currency, ?, ?)
    // 10 (8bit mantisa) 54 bit mantissa, 160 bit currency code, 160 bit account
    // If the amount is zero a special amount if returned... TODO: Check if correct

    // TODO: Deal with value of 0 using zeroFiatAmount
    // TODO: Potentically deal with currency of XRP by using xrpCurrencyCode (real case?)
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

  protected def encodeFullIOU(jobj: JsonObject): Either[BinCodecLibError, EncodedNestedValue] = {
    val noNulls = jobj.filter(_._2 != Json.Null)
    def encodeField(name: String, fn: Json => Either[BinCodecLibError, Encoded]): Either[BinCodecLibError, Encoded] = {
      findField(name, noNulls).flatMap(fn)
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
  def encodeCurrency(json: Json): Either[BinCodecLibError, RawValue] = {
    // TODO: Look what the first 2 bits and format are for pure hex.
    // This should always be 20 bytes long if ASCII psuedo-ISO code
    // 2 bit b00 for psuedo ISO type     b01 for other
    // 88-bit b0
    // 24-bit encoded ISO (1 byte per char)
    // 40-bit b0 padding
    // 20 bytes total
    val bit90Zero: List[UByte] = List.fill(12)(UByte(0))
    val bit40Zero: List[UByte] = List.fill(5)(UByte(0))
    val bit160Zero             = List.fill(20)(UByte(0))
    //  It should be a 160 bit hex string but can't be XRP

    json2string(json)
      .flatMap {
        case "XRP" => RawValue(bit160Zero).asRight
        case s if s.length === 3 && isRippleAscii(s) =>
          val curr = ByteUtils.bytes2ubytes(s.getBytes("UTF-8")).toList
          RawValue(bit90Zero ::: curr ::: bit40Zero).asRight

        case s if s.length === 40 => ByteUtils.hex2ubytes(s).map(RawValue.apply)

        case other => BinCodecLibError(s"Currency $other not three ascii").asLeft
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
