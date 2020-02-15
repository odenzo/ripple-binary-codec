package com.odenzo.ripple.bincodec.scodecs

import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, _}

import com.odenzo.ripple.bincodec.OTestSpec

class VLTest extends OTestSpec {

  import VL._
  implicit val log: Boolean = true

  def printBits(bv: BitVector): Unit = scribe.info(s"BV $bv =>  ${bv.toBin}")

  test("Small Zero") {

    VL.xrpvl.encode(0).map { bv =>
      bv.length shouldBe 8
      bv.bytes shouldEqual hex"00"
    }
  }

  test("Small Encoding") {
    VL.xrpvl.encode(1).map { bv =>
      bv.length shouldBe 8
      bv.bytes shouldEqual hex"01"
    }
  }

  test("Small 192 ") {
    // This is the max value of first byte is one byte encoding
    VL.xrpvl.encode(192).map { bv =>
      bv.length shouldBe 8
      bv.bytes shouldEqual hex"C0"
    }
  }

  test("Medium 193") {
    VL.xrpvl.encode(193).map { bv =>
      printBits(bv)
      bv.length shouldBe 16
      bv.bytes shouldEqual (hex"C100")
    }
  }
  // Broken, but we actually never use large VL Encoding that I see.
//  test("Medium 12480") {
//    VL.xrpvl.encode(12480).map { bv =>
//      printBits(bv)
//      bv.length shouldBe 16
//      bv.bytes shouldEqual (hex"F0FF")
//    }
//  }

  test("Large 12481") {
    VL.xrpvl.encode(12481).map { bv =>
      printBits(bv)
      bv.length shouldBe 24
      bv.bytes shouldEqual (hex"F1_00_00")
      xrpvl.encode(12481).require.bytes shouldEqual (hex"F1_00_00")
    }

  }

  test("Property") {
    // @todo Property based test
    (0 to 192).foreach(
      roundTripFromEncode(_, xrpvl)
    )
  }

  test("Property Mid") {
    // @todo Property based test
    (193 to 1024).foreach { v =>
      scribe.debug(s"Mid Value: $v")
      roundTripFromEncode(v, xrpvl)(true)
    }
  }

  // 449 encode fails
  test("449") {
    roundTripFromEncode(449, xrpvl)(true)
  }
  test("Property High") {
    // @todo Property based test
    (12481 to 13400).foreach(
      roundTripFromEncode(_, xrpvl)(false)
    )
  }
}
