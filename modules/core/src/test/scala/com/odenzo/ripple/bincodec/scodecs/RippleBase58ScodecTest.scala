package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import cats.implicits._
import scodec.bits._

class RippleBase58ScodecTest extends OTestSpec with STObjectScodec {

  import RippleBase58Scodec._
  val x   = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"
  val bin = hex"005e7b112523f68d2f5e879db4eac51c6698a69304fb95b2ee"
  test("Base58 to Hex") {
    roundTripScodec(bin.bits, x, xrplBase58)(true)
  }

  test("Special") {
    roundTripScodec(hex"0000000036".bits, "rrrrA", xrplBase58)(true)
  }

}
