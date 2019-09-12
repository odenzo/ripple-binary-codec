package com.odenzo.ripple.bincodec.utils

import scala.collection.immutable

import org.scalatest.FunSuite
import spire.math._

import com.odenzo.ripple.bincodec.{OTestSpec, BinCodecLibError}

class ByteUtilsTest extends FunSuite with OTestSpec {

  import ByteUtils._

  test("UByte to Hex") {

    ByteUtils.ubyte2hex(UByte(1)) shouldEqual "01"
    ByteUtils.ubyte2hex(UByte(255)).equalsIgnoreCase("ff") shouldBe true

    val fullRange: String = (0 to 255)
      .map { n =>
        val hex = ByteUtils.ubyte2hex(UByte(n.toByte))
        s"$n $hex"
      }
      .mkString("\n")
    scribe.debug("Full Range to Hex\n" + fullRange)
  }

  test("UByte 2 Looping") {
    (0 to 255).foreach { n =>
      val byte: UByte                           = UByte(n.toByte)
      val hex: String                           = ByteUtils.ubyte2hex(byte)
      val back: Either[BinCodecLibError, UByte] = ByteUtils.hex2ubyte(hex)

      scribe.debug(s"$byte <=> $hex <=> $back")
      getOrLog(back) shouldEqual byte

    }
  }

  test("Unsafe") {
    unsafeHex2ubytes("FACEBEEF") shouldEqual getOrLog(hex2ubytes("FACEBEEF"))

    val thrown = the[Exception] thrownBy unsafeHex2ubytes("1BADRABBIT")
    scribe.info("Thrown" + thrown)
  }

  test("HexToBytes") {
    val fixs: immutable.Seq[(String, UByte)] = (0 to 255).map { n =>
      val byte: UByte = UByte(n.toByte)
      val hex         = ubyte2hex(byte)
      hex.length shouldEqual 2
      (hex, byte)
    }
    val allHexDigits: String = fixs.map(_._1).mkString
    scribe.debug(s"All Fex $allHexDigits")
    val calc: List[UByte] = getOrLog(hex2ubytes(allHexDigits))
    calc.length shouldBe 256
  }
}
