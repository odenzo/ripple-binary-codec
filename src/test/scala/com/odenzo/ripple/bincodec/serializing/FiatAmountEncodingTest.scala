package com.odenzo.ripple.bincodec.serializing

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, Json}
import org.scalatest.{Assertion, FunSuite}
import spire.math._

import com.odenzo.ripple.bincodec.serializing.DebuggingShows._
import com.odenzo.ripple.bincodec.utils.caterrors.CodecError
import com.odenzo.ripple.bincodec.utils.{ByteUtils, CirceUtils}
import com.odenzo.ripple.bincodec.{OTestSpec, OTestUtils}

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
      val res = getOrLog(CurrencyEncoders.rippleEncodingOfFiatAmount(amt))
      logger.info(s"Res: $amt => ${res.toHex}")
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

  val parsed: Json               = getOrLog(CirceUtils.parseAsJson(fixture))
  val fixes: List[AmountFixture] = getOrLog(CirceUtils.decode(parsed, Decoder[List[AmountFixture]]))

  test("All") {

    fixes.take(3).map { v: AmountFixture ⇒
      logger.debug(s"Fixture $v")

      logger.info(s"0 ${ByteUtils.ubyte2hex(UByte(0))}")

      val res: BinarySerializer.RawEncodedValue = getOrLog(CurrencyEncoders.encodeFiatValue(v.value))
      logger.info(s"Res: ${res.show}")
      assert(res.ubytes.length == 8)
      logger.info(s"Got Res: ${res.toHex}")
      val hex   = res.toHex

      v.bin shouldEqual hex

      // NOTE: I return  32 bytes, expected 64 bytes. Maybe cause if zero currency or issuer is nuked.
      // See how this works on encoding a full FiatAmount
      Option(true)
    }

  }

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

    case class TData(bin: String, mant: String, exp: Int, value: String)

    val td: Either[CodecError, List[TData]] =
      CirceUtils
        .parseAsJson(fixture)
        .flatMap(j ⇒ CirceUtils.decode(j, Decoder[List[TData]]))

    CodecError.log(td, "Error Parsing Test Data: ")
    val testData: List[TData] = td.right.value

    testData.foreach { fix: TData ⇒
      val bd                  = BigDecimal(fix.value)
      val step1: (ULong, Int) = CurrencyEncoders.normalizeToIntegral(bd)
      logger.debug(s"Step 1 $step1")

      val res = CurrencyEncoders.normalizeAmount2MantissaAndExp(bd)
      CodecError.log(res)
      val (mant, exp) = res.right.value
      logger.info(s"Fully Normalized: $mant -> $exp")

      val fiatAmount                       = BigDecimal(fix.value)
      val amtRes: BinarySerializer.Encoded = getOrLog(CurrencyEncoders.rippleEncodingOfFiatAmount(fiatAmount))
      amtRes.toHex shouldEqual fix.bin

    }
  }
  def testOne(v: Json, expected: Json): Assertion = {
    val expectedHex                             = expected.asString.get
    val bytes: BinarySerializer.RawEncodedValue = getOrLog(CurrencyEncoders.encodeFiatValue(v))
    bytes.toHex shouldEqual expectedHex
  }

}
