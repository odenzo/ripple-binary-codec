package com.odenzo.ripple.bincodec.codecs

import cats.implicits._
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
trait MoneyCodecs extends CodecUtils with JsonUtils {

  private val maxXRP: BigInt = spire.math.pow(BigInt(10), BigInt(17))

  /**  0x8000000000000000000000000000000000000000 to represent special case encoding of 0 fiat amount xrp */
  // private val zeroFiatAmount: List[UByte] = UByte(0x80) :: List.fill(19)(UByte(0))

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

  /**
    *
    * @param v
    * @param info
    * @return XRP Hex with the 63th bit set to 1 (not checked) or 48 bytes of fiat amount
    */
  def decodeAmount(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (DecodedField, List[UByte])] = {

    val TOP_BIT_MASK: UByte = UByte(128)
    val SIGN_BIT_MASK       = ~UByte(64) // Sign Bit is always 1 for XRP

    v match {
      case h :: t if (h & TOP_BIT_MASK) === UByte(0) ⇒ decodeToUBytes(8, (h | SIGN_BIT_MASK) :: t, info) // XRP
      case other                                     ⇒ decodeToUBytes(48, other, info) // Fiat
    }

  }

  /** This is actually drops btw.**/
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
      value    ← findField("value", v).flatMap(IssuedAmountCodec.encodeFiatValue)
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
    // TODO: Look what the first 2 bits and format are for pure hex.
    // This should alwqays be 20 bytes long if ASCII psuedo-ISO code
    // 2 bit b00 for psuedo ISO type     *b01 for other
    // 88-bit b0
    // 24-bit encoded ISO (1 byte per char
    // 40-bit b0 padding
    // 19 bytes total
    val bit90Zero: List[UByte] = List.fill(12)(UByte(0))
    val bit40Zero: List[UByte] = List.fill(5)(UByte(0))
    //  It should be a 160 bit hex string but can't be XRP
    // The raw hex form in json can be handled, but can't be XRP
    val xrpHex = "0158415500000000C1F76FF6ECB0BAC600000000"

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
    * Used to check if ISO currency codes are ok.
    *
    * @param s
    *
    * @return true is valid
    */
  protected def isRippleAscii(s: String): Boolean = { s.forall(c ⇒ rippleAscii.contains(c)) }



}

object MoneyCodecs extends MoneyCodecs
