package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import cats.implicits._
import scodec.bits._

class RippleBase58ScodecTest extends OTestSpec with STObjectScodec {

  import RippleBase58Scodec._
  val x = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"

  test("Base58 to Hex") {
    val env: BitVector = roundTripFromEncode(xrplBase58, x)
  }

  test("Special") {
    val env: BitVector = roundTripFromEncode(xrplBase58, "rrrrA")

  }

}
