package com.odenzo.ripple.bincodec.scodecs

import cats.implicits._
import scodec.bits._
import scodec._

import com.odenzo.ripple.bincodec.OTestSpec

class AmountScodecsTest extends OTestSpec with AmountScodecs {
  val x = "r9cZA1mLK5R5Am25ArfXFmqgNwjZgnfk59"

  test("XRP Encode") {
    val xrp  = 12
    val bits = xprAmountEncFn(xrp).require
    scribe.info(s"Len: ${bits.size} -> ${bits.toHex}")
    val longV: DecodeResult[Long] = xprAmountDecFn(bits).require
    scribe.info(s"Decoding Result: $longV")
    val xrpBack = longV.value
    xrpBack shouldEqual xrp

    xrpAmountDec.encode(xrp).map(xrpAmountDec.decode).map(_.require.value shouldEqual xrp)
  }

}
