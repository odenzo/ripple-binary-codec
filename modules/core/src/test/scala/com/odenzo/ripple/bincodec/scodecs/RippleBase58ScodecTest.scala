package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import cats.implicits._
import scodec.bits._

class RippleBase58ScodecTest extends OTestSpec with STObjectScodec {

  import RippleBase58Scodec._
  val x = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"

  def roundtrip(in: String) = {
    val env: BitVector = roundTripFromEncode(xrplBase58,in)
  }

  test("Base58 to Hex") {
    roundtrip(x)
  }

  test("Special") {
    roundtrip("rrrrA")
  }

}
