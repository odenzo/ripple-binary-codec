package com.odenzo.ripple.bincodec.serializing

import io.circe.Json
import org.scalatest.FunSuite
import spire.math.ULong

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.utils.caterrors.AppError

class CurrencyEncodersTest extends FunSuite with OTestSpec {

  test("Fiat Amount") {
    val amt = BigDecimal("1.234")
    val res = CurrencyEncoders.rippleEncodingOfFiatAmount(amt)
    AppError.dump(res).foreach { e ⇒
      logger.error("Error: " + e)
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

  val justAmount = """{
                    |  "currency" : "USD",
                    |  "value" : "10",
                    |  "issuer" : "rMYBVwiY95QyUnCeuBQA1D47kXA9zuoBui"
                    |}"""

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
    val res      = getOrLog(CurrencyEncoders.encodeCurrency(Json.fromString(currency)))
    val hex      = res.toHex
    logger.debug(s"$currency => $hex")

    val amount: Json             = Json.fromString("10")
    val amountBytes: BinarySerializer.Encoded = getOrLog(CurrencyEncoders.encodeFiatValue(amount))
    val amountHex: String        = amountBytes.toHex
    logger.info(s"Amount: ${amount.spaces2} ⇒ $amountHex")

    val issuer: Json = Json.fromString(issuerStr)
    val issuerBytes  = AccountIdCodecs.encodeAccountNoVL(issuer).right.value
    val issuerHex    = issuerBytes.toHex
    logger.debug(s"Issuer [$issuer] Len ${issuerHex.length * 4} : $issuerHex")

    hex shouldEqual currencyExpected
    issuerHex shouldEqual issuerHex
    amountHex shouldEqual amountExpected
  }

  test("Normalizing") {

    val ans: Either[AppError, (ULong, Int)] = CurrencyEncoders.properNormalize(ULong(1L), 2)
    AppError.dump(ans).foreach(msg ⇒ logger.error("Error: " + msg))

  }

}
