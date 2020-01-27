package com.odenzo.ripple.bincodec.scodecs

import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import scodec.bits._

import com.odenzo.ripple.bincodec.OTestSpec

class VLTest extends OTestSpec {

  import VL._

  def printBits(bv: BitVector): Unit = scribe.info(s"BV $bv =>  ${bv.toBin}")

  test("Small Zero") {
    smallVL.encode(0).map { bv =>
      bv.length shouldBe 8
      bv.bytes shouldEqual hex"00"
    }
  }

  test("Small Encoding") {
    smallVL.encode(1).map { bv =>
      bv.length shouldBe 8
      bv.bytes shouldEqual hex"01"
    }
  }

  test("Small 192 ") {
    // This is the max value of first byte is one byte encoding
    smallVL.encode(192).map { bv =>
      bv.length shouldBe 8
      bv.bytes shouldEqual hex"C0"
    }
  }

  test("Medium 193") {
    mediumVL.encode(193).map { bv =>
      printBits(bv)
      bv.length shouldBe 16
      bv.bytes shouldEqual (hex"C101")
    }
  }
  test("Medium 12480") {
    mediumVL.encode(12480).map { bv =>
      printBits(bv)
      bv.length shouldBe 16
      bv.bytes shouldEqual (hex"F100")
    }
  }

  test("Large 12481") {
    largeVL.encode(12481).map { bv =>
      printBits(bv)
      bv.length shouldBe 24
      bv.bytes shouldEqual (hex"F1_00_00")
      xrpvl.encode(12481).require.bytes shouldEqual (hex"F1_00_00")
    }

  }

  test("VL Decoding") {

    val rs = xrpvl.encode(100000).map { rs =>
      val something                       = hex"0F3D0C7D2CFAB2EC8295451F0B3CA038E8E9CDCD".bits
      val res: Attempt[DecodeResult[Int]] = decodeVL.decode(something)

      //res shouldEqual "0f"

      xrpvl.encode(15).map { bv: BitVector =>
        bv.length shouldEqual 8
        bv shouldEqual hex"0F".bits
      }

      xrpvl.encode(1000).map { bv: BitVector =>
        bv.length shouldEqual 16
        bv shouldEqual hex"0F".bits
      }

      xrpvl.encode(1000).map { bv: BitVector =>
        bv.length shouldEqual 16
        bv shouldEqual hex"0F".bits
      }
    }
  }

}
