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
class IssuedAmountCodecTest extends OTestSpec {

  test("Fiat Amount Doesnt Die") {
    val fixture: Seq[String] = Seq(
      "10",
      "100",
      "100.23456",
      "0.12345",
      "0000.0012345600",
      "0000.12345",
      "0000.123450000"
    )

    fixture.foreach { txt =>
      val res = getOrLog(IssuedAmountCodec.encodeFiatValue(Json.fromString(txt)))
      scribe.info(s"Res: $txt => $res")
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

  /** Testing the edge cases where XRPL acts not exactly like I expected */
  test("Conformance") {
    // Need to make a decent conformance test set
    //
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

    for {
      json <- parseAsJson(fixture)
      data <- decode(json, Decoder[List[TData]])
      _ = data.foreach { fix =>
        val bin: RawValue = getOrLog(IssuedAmountCodec.encodeFiatValue(Json.fromString(fix.value)))
        bin.toHex == fix.bin
        bin.toHex shouldEqual fix.bin
      }
    } yield ()

  }

  def testOne(v: Json, expected: Json): Assertion = {
    val expectedHex = expected.asString.get
    val bytes       = getOrLog(IssuedAmountCodec.encodeFiatValue(v))
    bytes.toHex shouldEqual expectedHex
  }

}
