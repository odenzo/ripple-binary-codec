package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, Json}
import org.scalatest.{Assertion, FunSuite}
import scribe.Level
import spire.math._

import com.odenzo.ripple.bincodec.syntax.debugging._
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{OTestSpec, OTestUtils, RawValue, TestLoggingConfig}

/**
  * Trouble with Fiat encoding so a dedicated test suite.
  */
class FiatAmountEncodingTest extends FunSuite with OTestSpec with OTestUtils {

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
    val td: Either[RippleCodecError, List[TData]] =
      JsonUtils
        .parseAsJson(fixture)
        .flatMap(j => JsonUtils.decode(j, Decoder[List[TData]]))

    RippleCodecError.log(td, "Error Parsing Test Data: ")
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

}
