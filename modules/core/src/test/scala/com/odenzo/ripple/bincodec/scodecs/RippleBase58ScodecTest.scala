package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import cats.implicits._
import scodec.bits._

class RippleBase58ScodecTest extends OTestSpec with STObjectScodec {

  import RippleBase58Scodec._
  val x = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"

  test("Base58 to Hex") {
    roundTripScodec(hex"000".bits, x, xrplBase58)(true)
  }

  test("Special") {
    roundTripScodec(hex"000".bits, "rrrrA", xrplBase58)(true)
  }

}
