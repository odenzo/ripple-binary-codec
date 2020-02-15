package com.odenzo.ripple.bincodec.scodecs

import scodec.codecs._
import scodec.bits.{HexStringSyntax, _}
import scodec.{Codec, _}
import spire.math.UInt

trait VL {

  private val smallMask = hex"F000"

  protected val smallVL: Codec[Int] = uint8.withContext("Small VL")

  protected val mediumVL: Codec[Int] = (uint8 ~ uint8)
    .xmap[Int](
      (bt: (Int, Int)) => 193 + ((bt._1 - 193) * 256) + bt._2,
      (i: Int) => {
        val l2: UInt = UInt(i - 193)
        val b1: UInt = UInt(193) + (l2 >>> 8)
        val b2: UInt = l2 & UInt(0xff)
        (b1.toInt, b2.toInt)
      }
    )
    .withContext("Medium VL")

  protected val largeVL: Codec[Int] = (uint8 ~ uint8 ~ uint8)
    .xmap[Int](
      (bt: ((Int, Int), Int)) => 12481 + ((bt._1._1 - 241) * 65536) + (bt._1._2 * 256) + bt._2,
      (i: Int) => {
        val lenA: Int = i - 12481
        val b1        = 241 + (lenA >>> 16)
        val b2        = (lenA >> 8) & 0xFF
        val b3        = lenA & 0xFF
        ((b1, b2), b3)
      }
    )
    .withContext("Large VL")

  /** This encodes 0 as valid length */
  protected def encodeVlAttempt(len: Int): Attempt[BitVector] = {
    len match {
      case len if len < 0       => Attempt.failure(Err(s"$len was less than 0"))
      case len if len <= 192    => smallVL.encode(len)
      case len if len <= 12480  => mediumVL.encode(len)
      case len if len <= 918744 => largeVL.encode(len)
      case _                    => Attempt.failure(Err(s"$len was > 918744"))
    }
  }

  val encodeVL: Encoder[Int] = Encoder[Int](encodeVlAttempt _)

  protected def decodeVL: Decoder[Int] = {
    peek(uint(8)).flatMap { x: Int =>
      scribe.debug(s"DecodeVL on First Byte:  $x")
      val len = x match {
        case l if l < 0    => fail[Int](Err(s"Marker Bytes too Small $l"))
        case l if l <= 192 => smallVL
        case l if l <= 240 => mediumVL
        case l if l <= 255 => largeVL
        case l             => fail[Int](Err(s"First Byte   $l = 255  "))
      }

      len
    }
  }

  /** This will return the VL encoding in length in bytes (not bits or hex nibble)
    * Make sure no duplicate VL at field and internal level */
  val xrpvl: Codec[Int] = Codec[Int](encodeVL, decodeVL)
}

object VL extends VL
