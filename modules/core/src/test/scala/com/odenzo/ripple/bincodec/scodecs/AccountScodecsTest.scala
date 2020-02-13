package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import cats.implicits._
import scodec.bits._
import scodec._

class AccountScodecsTest extends OTestSpec with AccountScodecs {

  test("Address Decoder") {
    val binary                          = hex"5e7b112523f68d2f5e879db4eac51c6698a69304"
    val model                           = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"
    val decResult: DecodeResult[String] = roundTripScodec(binary.bits, model, xrplAccount)
    decResult.remainder shouldBe empty
  }

  val vlAcct1 = hex"81_14_0864D99FE19C6B19B0B7BA865B9A4A552173A896"
  val vlAcct2 = hex"83_14_D91693CB3D87723F716A16C46A398B9659864B0F"
  val acct1   = "rmPD5tJXdk3h4guoCsNADeDXRzmjvG3Ez"
  val acct2   = "rL8igKxCefdw8Jnmp2W4wpmgKjTfu1seKA"

  test("Leading Zero Account") {
    val decResult: DecodeResult[String] = roundTripScodec(vlAcct1.drop(2).bits, acct1, xrplAccount)
    decResult.remainder shouldBe empty
  }

  test("Acct2") {
    val decResult: DecodeResult[String] = roundTripScodec(vlAcct2.drop(2).bits, acct2, xrplAccount)
    decResult.remainder shouldBe empty
  }

  test("Encode") {
    val expected = vlAcct1.drop(2)
    scribe.debug(s"A1 B58: $acct1")
    scribe.debug(s"A1 Hex: $expected")
    val encBits: BitVector = xrplAccount.encode(acct1).require
    scribe.debug(s"A1 Encoded: $encBits")
    encBits shouldEqual expected.bits
  }

  test("Decode") {
    // Drop the FieldId and the VL Encoding
    val acctOnly = vlAcct1.drop(2)
    scribe.debug(s"Decoding Acct: ${acctOnly.toHex}")
    val dec: DecodeResult[String] = xrplAccount.decode(acctOnly.bits).require
    scribe.debug(s"Decoded: $dec")
    dec.value shouldEqual acct1
  }
}
