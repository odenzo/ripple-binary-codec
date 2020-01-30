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
    val drops = xprAmountDecFn(bits).require
    scribe.info(s"Decoding Result: $drops")
    val xrpBack = drops.value
    xrpBack shouldEqual xrp

    xrpXrpAmount.encode(XRPLDrops(xrp)).map(xrpXrpAmount.decode).map(_.require.value shouldEqual xrp)
  }

  test("Currency") {
    // Not sure this is exactly aligned  This should be fourty characters exactly, may be slidden by 1 byte
    val nzd: ByteVector = hex"0000000000000000000000004e5a440000000000"
    val res             = xrplCurrency.decode(nzd.bits).require
    scribe.info(s"Result: $res")
  }
}
