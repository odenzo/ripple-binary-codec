package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Json, Decoder}
import org.scalatest.Assertion
import scribe.Level
import spire.math._

import com.odenzo.ripple.bincodec.testkit.TestLoggingConfig
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{OTestSpec, RawValue, BinCodecLibError}

/**
  * Trouble with Fiat encoding so a dedicated test suite.
  */
class FiatAmountEncodingTest extends OTestSpec {

  test("Fiat Amount") {
    val fixture: Seq[BigDecimal] = Seq(
      BigDecimal(10),
      BigDecimal("100"),
      BigDecimal("100.23456"),
      BigDecimal("0.12345"),
      BigDecimal("0000.0012345600"),
      BigDecimal("0000.12345"),
      BigDecimal("0000.123450000")
    )

    fixture.foreach(oneOff)
    def oneOff(amt: BigDecimal): Unit = {
      val res = getOrLog(IssuedAmountCodec.newEncodeFiatAmount(amt))
      scribe.info(s"Res: $amt =>")
    }
  }

  // Is the 0 value just for XRP, failing needed to double check
  private val fixture =
    """
      |[
      | { "bin": "8000000000000000" , "value": "0" }  ,
      | { "bin": "D4C38D7EA4C68000" , "value": "10" }  
      |]
    """.stripMargin

  import io.circe.generic.auto._

  case class ByField(topBits: ULong, exp: ULong, mantissa: ULong)
  case class AmountFixture(value: Json, bin: String, passed: Option[Boolean] = None)

  val parsed: Json               = getOrLog(JsonUtils.parseAsJson(fixture))
  val fixes: List[AmountFixture] = getOrLog(JsonUtils.decode(parsed, Decoder[List[AmountFixture]]))

  test("All") {

    fixes.take(3).map { v: AmountFixture =>
      scribe.debug(s"Fixture $v")

      scribe.info(s"0 ${ByteUtils.ubyte2hex(UByte(0))}")

      val res = getOrLog(IssuedAmountCodec.encodeFiatValue(v.value))
      scribe.info(s"Res: ${res.show}")
      assert(res.ubytes.length == 8)
      scribe.info(s"Got Res: ${res.toHex}")
      val hex = res.toHex

      v.bin shouldEqual hex

      // NOTE: I return  32 bytes, expected 64 bytes. Maybe cause if zero currency or issuer is nuked.
      // See how this works on encoding a full FiatAmount
      Option(true)
    }

  }

  case class TData(bin: String, mant: String, exp: Int, value: String)

  test("Dev Fiat Amount Value") {
    val fixture =
      """
        |[
        | {"bin":"94838D7EA4C68000" ,"mant": "00038D7EA4C68000" , "exp": -15, "value": "-1.0" },
        |
        | {"bin":"8000000000000000" ,"mant": "0000000000000000" , "exp": -15, "value": "0.0" },
        |
        | {"bin":"D4C3F28CB71571C7" ,"mant": "0003F28CB71571C7" , "exp": -14, "value": "11.11111111111111" }
        |]
      """.stripMargin

    TestLoggingConfig.setAll(Level.Debug)
    val td: Either[BinCodecLibError, List[TData]] =
      JsonUtils
        .parseAsJson(fixture)
        .flatMap(j => JsonUtils.decode(j, Decoder[List[TData]]))

    val testData: List[TData] = getOrLog(td)

    // Note that I return (0,0) for value 0.0 but encodes the same
    testData.foreach { fix: TData =>
      val fiatAmount    = BigDecimal(fix.value)
      val bin: RawValue = getOrLog(IssuedAmountCodec.encodeFiatValue(Json.fromString(fix.value)))
      bin.toHex shouldEqual fix.bin
    }
  }
  def testOne(v: Json, expected: Json): Assertion = {
    val expectedHex = expected.asString.get
    val bytes       = getOrLog(IssuedAmountCodec.encodeFiatValue(v))
    bytes.toHex shouldEqual expectedHex
  }

  /**
    *
    * @param got      Value of field, without field marker
    * @param expected Expected value of fields without field marker
    *
    * @return
    */
  def analyzeAmount(got: String, expected: String): Either[BinCodecLibError, Unit] = {

    val isXRP: Either[BinCodecLibError, Boolean] = ByteUtils
      .hex2ubyte("0" + got.drop(2).head)
      .map(b => (b | UByte(8)) === UByte(0))

    isXRP.map {
      case true => scribe.info("XRP Value Deal With IT")
      case false =>
        scribe.info("Analyzing Suspected Fiat Amount")
        val gotFields: Either[BinCodecLibError, (List[UByte], List[UByte], List[UByte])]      = breakFiat(got)
        val expectedFields: Either[BinCodecLibError, (List[UByte], List[UByte], List[UByte])] = breakFiat(expected)
    }

  }

  /** Breaks down to UBytes for the amount, currency amd issuer */
  def breakFiat(hex: String): Either[BinCodecLibError, (List[UByte], List[UByte], List[UByte])] = {

    val all: Either[BinCodecLibError, List[UByte]] = ByteUtils.hex2ubytes(hex)
    val amount                                     = all.map(_.take(8)) // Top 64 is amount in sign and flag
    val currency                                   = all.map(_.slice(8, 28)) // 160 bits
    val issuer                                     = all.map(_.slice(32, 52)) // another 160 bits
    (amount, currency, issuer).mapN((_, _, _))
  }

  /** Get Top 2 bits, Exponent (Shifted) anf the mantissa in that order in a list.
    * Moved down so the 2 bits in ULong has value 3 is both set etc.
    * */
  def breakFiatAmount(fields: ULong): List[ULong] = {

    // We char about the first 10 bits contains in the first two bytes

    val topMask: ULong      = ULong(0xC000000000000000L)
    val expMask: ULong      = ULong(0xFF) << 54
    val mantissaMask: ULong = ULong(0x3FFFFFFFFFFFFFL) // 13 nibbles

    //    scribe.debug("Masks:\n" + ByteUtils.uLong2Base2Str(topMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(expMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(mantissaMask))

    val top2     = (fields & topMask) >> 62
    val exp      = (fields & expMask) >> 54
    val mantissa = fields & mantissaMask

    List(top2, exp, mantissa)
  }
}
