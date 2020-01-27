package com.odenzo.ripple.bincodec.scodecs

import spire.math.{UByte, UInt}
import scodec.codecs._
import scodec.Codec
import scodec.bits._
import scodec.bits.HexStringSyntax
import scodec._

import com.odenzo.scodec.spire._

object VL {

  val smallMask = hex"F000"

  val smallVL: Codec[Int] = uint8.withContext("Small VL")

  val mediumVL: Codec[Int] = (uint8 ~ uint8)
    .xmap[Int](
      (bt: (Int, Int)) => 193 + ((bt._1 - 193) * 256) + bt._2,
      (i: Int) => {
        val l2      = i - 192
        val b1: Int = 193 + (l2 >>> 8)
        val b2: Int = l2 & 0xFF
        (b1, b2) // Only output 2 bytes
      }
    )
    .withContext("Medium VL")

  val largeVL: Codec[Int] = (uint8 ~ uint8 ~ uint8).xmap[Int](
    (bt: ((Int, Int), Int)) => 241 + bt._1._1 + bt._1._2 + bt._2,
    (i: Int) => {
      val lenA: Int = i - 12481
      val b1        = 241 + (lenA >>> 16)
      val b2        = (lenA >> 8) & 0xFF
      val b3        = lenA & 0xFF
      ((b1, b2), b3)
    }
  )

  /** Until I figure out the variable length combinator */
  def encodeVlAttempt(len: Int): Attempt[BitVector] = {
    len match {
      case len if len <= 0      => Attempt.failure(Err(s"$len was less than 0"))
      case len if len <= 92     => smallVL.encode(len)
      case len if len <= 12481  => mediumVL.encode(len)
      case len if len <= 918744 => largeVL.encode(len)
      case _                    => Attempt.failure(Err(s"$len was > 918744"))
    }
  }

  val encodeVL: Encoder[Int] = Encoder[Int](encodeVlAttempt _)

  def decodeVL: Decoder[Int] = {
    peek(uint(8)).flatMap { x: Int =>
      x match {
        case l if l < 0    => fail[Int](Err(s"Marker Bytes too Small $l"))
        case l if l <= 192 => smallVL
        case l if l <= 240 => mediumVL
        case l if l <= 255 => largeVL
        case l             => fail[Int](Err(s"First Byte   $l = 255  "))
      }
    }
  }

  /** This will return the VL encoding in length in bytes (not bits or hex nibble)
    * Make sure no duplicate VL at field and internal level */
  val xrpvl = Codec[Int](encodeVL, decodeVL)
}