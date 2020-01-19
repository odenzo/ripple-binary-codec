package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterAll
import spire.math._
import spire.implicits._
import io.circe.literal._

import com.odenzo.ripple.bincodec.OTestSpec
import scodec.interop.cats._
import scodec.bits._
class MoneyCodecsTest extends OTestSpec with BeforeAndAfterAll {

  // Ripple method working, but we can make more concise using Java Math
  // This tries to compare the two.

  test("Fixture") {

    val f =
      hex"110072220002000025000000EF55C6A2521BBCCF13282C4FFEBC00D47BBA18C6CE5F5E4E0EFC3E3FCE364BAFC6B862800000000000000000000000000000000000000055534400000000000000000000000000000000000000000000000001668000000000000000000000000000000000000000555344000000000036D16F18B3AAC1868C1E3E8FA8EB7DDFD8ECCCAC67D4C38D7EA4C680000000000000000000000000005553440000000000E14829DB4C6419A8EFCAC1EC21D891A1A4339871"

    val zeroField: ByteVector =
      hex"110072220002000025000000EF55C6A2521BBCCF13282C4FFEBC00D47BBA18C6CE5F5E4E0EFC3E3FCE364BAFC6B862800000000000000000000000000000000000000055534400000000000000000000000000000000000000000000000001668000000000000000000000000000000000000000555344000000000036D16F18B3AAC1868C1E3E8FA8EB7DDFD8ECCCAC67D4C38D7EA4C680000000000000000000000000005553440000000000E14829DB4C6419A8EFCAC1EC21D891A1A4339871"

    val lastField =
      hex"""
             110072220002000025000000EF55C6A2521BBCCF13282C4FFEBC00D47BBA18C6CE5F5E4E0EFC3E3FCE364BAFC6B8
             62800000000000000000000000000
             0000000000000555344000000000
             00000000000000000000000000000000000000001
             668000000000000000000000000000
             0000000000005553440000000000
             36D16F18B3AAC1868C1E3E8FA8EB7DDFD8ECCCAC
             67
             D4C38D7EA4
             C680000000000000000000000000005553440000000000E14829DB4C6419A8EFCAC1EC21D891A1A4339871"""

  }

  val justAmount =
    json"""{
            "currency" : "USD",
            "value" : "10",
            "issuer" : "rMYBVwiY95QyUnCeuBQA1D47kXA9zuoBui"
          }"""

  val nzAmount =
    json"""{
            "value" : "555.666",
            "currency" : "NZD",
            "issuer" : "rwjH4461qQTa4fksY8doTLRdqKxDyd5nsi"
          }
    """

  test("NZ Amount") {
    val iou    = nzAmount
    val iouEnc = getOrLog(MoneyCodecs.encodeIOU(iou))
    val amtEnc = getOrLog(MoneyCodecs.encodeAmount(iou))
    scribe.info(s"Encoded IOU: ${iouEnc.show}")
    iouEnc shouldEqual amtEnc
  }

  test("Fiat Package") {

    val issuerStr = "rMYBVwiY95QyUnCeuBQA1D47kXA9zuoBui"
    val i2Str     = "rMBzp8CgpE441cp5PVyA9rpVV7oT8hP3ys"
    val expected = "67" +
      "D4C38D7EA4C68000" +
      "0000000000000000000000005553440000000000" + // Currency
      "E14829DB4C6419A8EFCAC1EC21D891A1A4339871"   // Issuer with no VL Encoding

    val fieldExpected    = "67"
    val amountExpected   = "D4C38D7EA4C68000"
    val currencyExpected = "0000000000000000000000005553440000000000"
    val issuerExpected   = "E14829DB4C6419A8EFCAC1EC21D891A1A4339871" // Issuer with no VL Encoding

    val currency = "USD"
    val res      = getOrLog(MoneyCodecs.encodeCurrency(Json.fromString(currency)))
    val hex      = res.toHex
    scribe.debug(s"$currency => $hex")

    val amount: Json      = Json.fromString("10")
    val amountBytes       = getOrLog(IssuedAmountCodec.encodeFiatValue(amount))
    val amountHex: String = amountBytes.toHex
    scribe.info(s"Amount: ${amount.spaces2} => $amountHex")

    val issuer: Json = Json.fromString(issuerStr)
    val issuerBytes  = getOrLog(AccountIdCodecs.encodeAccountNoVL(issuer))
    val issuerHex    = issuerBytes.toHex
    scribe.debug(s"Issuer [$issuer] Len ${issuerHex.length * 4} : $issuerHex")

    hex shouldEqual currencyExpected
    issuerHex shouldEqual issuerHex
    amountHex shouldEqual amountExpected
  }

  test("XRP") {

    val min: ULong = ULong(0)
    val max: ULong = ULong.fromBigInt(spire.math.pow(BigInt(10), BigInt(17)))
    scribe.info(s"Min - Max $min $max")

    t(max)
    t(max - ULong(1))
    t(ULong(370000000))

    def t(v: ULong): String = {
      val jsonV: String = v.toString()
      val json: Json    = Json.fromString(jsonV)
      scribe.info(s"From $v Sending JSON: ${json.noSpaces}")
      val res = getOrLog(MoneyCodecs.encodeXrpAmount(json))
      val hex = res.toHex
      scribe.debug(s"$v  => $hex")
      hex
    }

  }

  test("XRP Encode") {
    val xrp: Json = Json.fromString("10000")
    val res       = getOrLog(MoneyCodecs.encodeXrpAmount(xrp))
    scribe.info(s"XRP ${xrp.noSpaces}  => ${res.toHex}")
  }

  test("Currency Encoding") {

    val passFail = List(
      ("XRP", true), // This is special case
      ("mew", true),
      ("FOO", true),
      ("BARR", false),
      ("~~~", false), // Not a ripple ASCII char
      ("[^]", true),
      ("AA".padTo(20, 'F'), false),
      ("00".padTo(20, 'F'), true),
      ("01".padTo(20, 'F'), true) // Legacy case
    )

    passFail.foreach { case (v, exp) => encode(v, exp) }

    def encode(v: String, expectedOK: Boolean): Assertion = {
      val res = MoneyCodecs.encodeCurrency(Json.fromString(v))
      logger.debug(s"$v expected $expectedOK got  $res")

      res.isRight shouldEqual expectedOK
    }

  }
}
