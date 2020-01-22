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

  def discEnc(l: Int): Codec[Int] = {
    scribe.info(s"Discriminating on $l for Enc")
    l match {
      case l if l <= 192    => smallVL.withContext("S Enc")
      case l if l <= 12480  => mediumVL.withContext("M Enc")
      case l if l <= 918744 => largeVL.withContext("L End")

      case l => fail[Int](Err(s"Variable Length to Encode  $l > 918744  "))
    }
  }

  def discDec(l: Int): Codec[Int] = {
    l match {
      case l if l <= 192 => smallVL
      case l if l <= 240 => mediumVL
      case l if l <= 255 => largeVL

      case l => fail[Int](Err(s"First Byte   $l = 255  "))
    }
  }
  val x = 12

  def fn(x: Int): Decoder[Int] =
    choice[Option[Int]](
      conditional(x >= 0 & x <= 192, provide(1)),
      conditional(x >= 193 & x <= 12480, provide(2)),
      conditional(x >= 12481 & x <= 918744, provide(3))
    ).asDecoder.emap {
      case Some(i) => Attempt.Successful(i)
      case None    => Attempt.Failure(Err(s"Could get byte size for $x"))
    }

//  val base = variableSizeBytes( ).consume(discEnc)((x: Int) => {
//      scribe.info(s"Reverse Mapping $x")
//
//    }
//                                    )
//
//  val vlEnc: Encoder[Int] =  limitedSizeBits(3,)
//    .asEncoder
//
//  // Because we start with uint16 we are always getting 16 bits back
//
//  // Not quite
//
//  val vlEnc2: GenCodec[Long, BitVector] = limitedSizeBytes[A](limit: Long, codec: Codec[A])vlong.emap(l => discEnc(l.toInt).encode(l.toInt))
//  val vlDec: Decoder[Int]               = uint8.consume(discDec)(identity).asDecoder
//

  //val vl = Codec(vlEnc2, vlDec)

  /** Until I figure out the variable length combinator */
  def encodeVL(len: UInt): Attempt[BitVector] = {
    len match {
      case len if len <= UInt(92)     => smallVL.encode(len.toInt)
      case len if len <= UInt(12481)  => mediumVL.encode(len.toInt)
      case len if len <= UInt(918744) => largeVL.encode(len.toInt)
      case other                      => Attempt.Failure(Err(s"len was > 918744"))
    }
  }

}
