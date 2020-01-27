package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import cats.implicits._
import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.codecs.implicits._

class AccountScodecsTest extends OTestSpec with AccountScodecs {

  import RippleBase58Scodec._
  val x = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"

  test("Address Encode") {
    xrpaccount.encode(x).map { r =>
      scribe.info(s"Result $r")
    }
  }

  // Note the checksum will not match
  test("Address Decoder") {
    val binary = hex"0x5e7b112523f68d2f5e879db4eac51c6698a69304".bits
    val res    = xrpaccount.decode(binary)
    scribe.info(s"Res $res")
  }

  test("RoundTrip") {
    val x                         = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"
    val enc: BitVector            = xrpaccount.encode(x).require
    val dec: DecodeResult[String] = xrpaccount.decode(enc).require
    x shouldEqual dec.value
  }
}
