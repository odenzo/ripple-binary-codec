package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import spire.math.ULong

import com.odenzo.ripple.bincodec.{Decoded, OTestSpec, RawValue}
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import io.circe.syntax._
import scribe.Level

class MoneyCodecsTest extends OTestSpec with BeforeAndAfterAll {

  // Ripple method working, but we can make more concise using Java Math
  // This tries to compare the two.

  import com.odenzo.ripple.bincodec.syntax.debugging._

  override def beforeAll(): Unit = {
    if (!com.odenzo.ripple.bincodec.inCI) {
      com.odenzo.ripple.bincodec.setAllToLevel(Level.Debug)
    }
  }

  override def afterAll(): Unit = {
    if (!com.odenzo.ripple.bincodec.inCI) {
      com.odenzo.ripple.bincodec.setAllToLevel(Level.Warn)
    }
  }


  test("Fixture") {

    val f =
      "110072220002000025000000EF55C6A2521BBCCF13282C4FFEBC00D47BBA18C6CE5F5E4E0EFC3E3FCE364BAFC6B862800000000000000000000000000000000000000055534400000000000000000000000000000000000000000000000001668000000000000000000000000000000000000000555344000000000036D16F18B3AAC1868C1E3E8FA8EB7DDFD8ECCCAC67D4C38D7EA4C680000000000000000000000000005553440000000000E14829DB4C6419A8EFCAC1EC21D891A1A4339871"

    val zeroField =
      "110072220002000025000000EF55C6A2521BBCCF13282C4FFEBC00D47BBA18C6CE5F5E4E0EFC3E3FCE364BAFC6B862800000000000000000000000000000000000000055534400000000000000000000000000000000000000000000000001668000000000000000000000000000000000000000555344000000000036D16F18B3AAC1868C1E3E8FA8EB7DDFD8ECCCAC67D4C38D7EA4C680000000000000000000000000005553440000000000E14829DB4C6419A8EFCAC1EC21D891A1A4339871"

    val lastField =
      "110072220002000025000000EF55C6A2521BBCCF13282C4FFEBC00D47BBA18C6CE5F5E4E0EFC3E3FCE364BAFC6B8" +
        "62800000000000000000000000000" +
        "0000000000000555344000000000" +
        "00000000000000000000000000000000000000001" +
        "668000000000000000000000000000" +
        "0000000000005553440000000000" + // USD Currency
        "36D16F18B3AAC1868C1E3E8FA8EB7DDFD8ECCCAC" +
        "67" +
        "D4C38D7EA4" +
        "C680000000000000000000000000005553440000000000E14829DB4C6419A8EFCAC1EC21D891A1A4339871"

  }

  val justAmount =
    """{
      |  "currency" : "USD",
      |  "value" : "10",
      |  "issuer" : "rMYBVwiY95QyUnCeuBQA1D47kXA9zuoBui"
      |}""".stripMargin

  val nzAmount =
    """{
      |
      |  "value" : "555.666",
      |  "currency" : "NZD",
      |  "issuer" : "rwjH4461qQTa4fksY8doTLRdqKxDyd5nsi"
      |}
    """.stripMargin

  test("NZ Amount") {
    val iou    = getOrLog(JsonUtils.parseAsJsonObject(nzAmount))
    val iouEnc = MoneyCodecs.encodeIOU(iou)
    val amtEnc = MoneyCodecs.encodeAmount(iou.asJson)
    scribe.info(s"Encoded IOU: ${iouEnc.show}")
    iouEnc shouldEqual amtEnc
  }

  test("Fiat Package") {

    val issuerStr = "rMYBVwiY95QyUnCeuBQA1D47kXA9zuoBui"
    val i2Str     = "rMBzp8CgpE441cp5PVyA9rpVV7oT8hP3ys"
    val expected = "67" +
      "D4C38D7EA4C68000" +
      "0000000000000000000000005553440000000000" + // Currency
      "E14829DB4C6419A8EFCAC1EC21D891A1A4339871" // Issuer with no VL Encoding

    val fieldExpected    = "67"
    val amountExpected   = "D4C38D7EA4C68000"
    val currencyExpected = "0000000000000000000000005553440000000000"
    val issuerExpected   = "E14829DB4C6419A8EFCAC1EC21D891A1A4339871" // Issuer with no VL Encoding

    val currency = "USD"
    val res      = getOrLog(MoneyCodecs.encodeCurrency(Json.fromString(currency)))
    val hex      = res.toHex
    scribe.debug(s"$currency => $hex")

    val amount: Json      = Json.fromString("10")
    val amountBytes       = getOrLog(FiatAmountCodec.encodeFiatValue(amount))
    val amountHex: String = amountBytes.toHex
    scribe.info(s"Amount: ${amount.spaces2} ⇒ $amountHex")

    val issuer: Json = Json.fromString(issuerStr)
    val issuerBytes  = AccountIdCodecs.encodeAccountNoVL(issuer).right.value
    val issuerHex    = issuerBytes.toHex
    scribe.debug(s"Issuer [$issuer] Len ${issuerHex.length * 4} : $issuerHex")

    hex shouldEqual currencyExpected
    issuerHex shouldEqual issuerHex
    amountHex shouldEqual amountExpected
  }


}
