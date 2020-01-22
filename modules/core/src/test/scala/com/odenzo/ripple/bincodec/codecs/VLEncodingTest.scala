package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import scodec.bits._
import spire.math.UInt
import com.odenzo.ripple.bincodec.OTestSpec

class VLEncodingTest extends OTestSpec with VLEncodingOldBroken {

  import org.scalacheck.Gen
  val hexGen = Gen.hexStr

  /** This is interesting test because I am not sure the true functionality, pad left with 0 to meet the expected size? */
  test("VL Small") {
    expectSuccess(encodeVL(0)) { bv =>
      bv.length shouldEqual 1
      bv shouldEqual hex"00"
    }
  }

  test("VL Small Max") { // I think 192 should go to two bytes
    expectSuccess(encodeVL(192)) { bv =>
      import spire.math.UByte
      scribe.debug(s"Bits: ${bv.bits.toBin}")
      bv.length shouldEqual 1
      bv shouldEqual ByteVector(UByte(192).byteValue())
    }
  }

  test("VL Medium") { // I think 192 should go to two bytes
    expectSuccess(encodeVL(193)) { bv =>
      import spire.math.UByte
      scribe.debug(s"Bits: ${bv.bits.toBin}")
      bv.length shouldEqual 2

    }
  }

  test("VL Property Style Almost") {
    List(0, 192, 193, 239, 240, 231, 500, 600, 700, 12479, 12480, 12481, 13000, 16000, 918744).foreach { len =>
      encodeVL(len) match {
        case Right(v)  => verifyVL(UInt(len), v)
        case Left(err) => fail(s"Encoding of $len failed $err")
      }
    }
  }

  def extract(i: Int, bv: ByteVector) = {
    scribe.debug(s"Extracting $i from ${bv.toHex}")
    bv.lift(i).map { b =>
      import spire.math.UByte
      val ub       = UByte(b)
      val ui: UInt = UInt.apply(ub.toInt)
      scribe.debug(s"As UByte $ub  and Uint  $ui")
      ui
    }
  }

  def verifyVL(len: UInt, res: ByteVector) = {
    logger.debug(s"Verify: $len ${res.toHex}")
    /*
     *
     * If the first length byte has a value of 192 or less, then that's the only length byte and it contains the exact length of the field contents in bytes.
     * (Top 4 bit 1100 or less)
     * If the first length byte has a value of 193 to 240, then there are two length bytes.
     * If the first length byte has a value of 241 to 254, then there are three length bytes.  (Top 4 bits 1)
     */
    val first = extract(0, res)
    scribe.info(s"First: $first  from ${res.toHex}")
    first.map(_.toInt).map {
      case l if l <= 192 => res.size shouldEqual 1
      case l if l <= 240 => res.size shouldEqual 2
      case l if l <= 254 => res.size shouldEqual 3
      case other         => fail(s"$other was illegal number of bytes")
    }
    /* If the field contains 0 to 192 bytes of data, the first byte defines the length of the contents, then that many bytes of data
                                                                                                     follow immediately after the length byte.
     *  If the field contains 193 to 12480 bytes of data, the first two bytes indicate the length of the field with the following formula:
     *    193 + ((byte1 - 193) * 256) + byte2
     *  If the field contains 12481 to 918744 bytes of data, the first three bytes indicate the length of the field with the following formula:
     *    12481 + ((byte1 - 241) * 65536) + (byte2 * 256) + byte3
     *
     */

    val undoLen = res.size match {
      case 1 => extract(0, res)
      case 2 =>
        List(0, 1)
          .traverse(extract(_, res))
          .map {
            case b2 :: b1 :: Nil => UInt(193) + ((b1 - UInt(193)) * UInt(256)) + b2
          }

      case 3 =>
        List(2, 1, 0).traverse(extract(_, res)).map {
          case b3 :: b2 :: b1 :: Nil =>
            UInt(12481) + ((b1 - UInt(241)) * UInt(65536)) + (b2 * UInt(256)) + b3
        }
    }

  }
}
