package com.odenzo.ripple.bincodec.scodecs

import scala.annotation.tailrec

import scodec.bits.Bases.{Alphabet, Alphabets}
import scodec.bits.{Bases, BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Encoder, Err}

import com.odenzo.ripple.bincodec.scodecs.RippleBase58Alphabet.rippleAlphabet

object RippleBase58Scodec {

  val xrpbase58 = utf8.exmap[BitVector](rippleEnc, rippleDec)

  def rippleEnc(str: String): Attempt[BitVector] = fromBase58Descriptive(str, rippleAlphabet) match {
    case Left(value)  => Attempt.failure(Err(value))
    case Right(value) => Attempt.successful(value.bits)
  }

  def rippleDec(bv: BitVector): Attempt[String] = Attempt.successful(toBase58(bv.bytes, rippleAlphabet))

  val xrpBase58Enc = Encoder[String](rippleEnc _)

  final def toBase58(bv: ByteVector, alphabet: Bases.Alphabet = rippleAlphabet): String = {

    val zeroChar = alphabet.toChar(0)
    if (bv.isEmpty) {
      ""
    } else {
      val ZERO  = BigInt(0)
      val RADIX = BigInt(58L)
      val ones  = List.fill(bv.takeWhile(_ == 0).length.toInt)(zeroChar)

      @tailrec
      def go(value: BigInt, chars: List[Char]): String = value match {
        case ZERO => (ones ++ chars).mkString
        case _ =>
          val (div, rem) = value /% RADIX
          go(div, alphabet.toChar(rem.toInt) +: chars)
      }

      go(BigInt(1, bv.toArray), List.empty)
    }

  }

  def fromBase58Descriptive(str: String, alphabet: Bases.Alphabet = Bases.Alphabets.Base58): Either[String, ByteVector] = {
    val zeroChar   = alphabet.toChar(0)
    val zeroLength = str.takeWhile(_ == zeroChar).length
    val zeroes     = ByteVector.fill(zeroLength.toLong)(0)
    val trim       = str.splitAt(zeroLength)._2.toList
    val RADIX      = BigInt(58L)
    try {
      val decoded = trim.foldLeft(BigInt(0)) { (a, c) =>
        try {
          a * RADIX + BigInt(alphabet.toIndex(c))
        } catch {
          case e: IllegalArgumentException =>
            val idx = trim.takeWhile(_ != c).length
            throw new IllegalArgumentException(s"Invalid base 58 character '$c' at index $idx")
        }
      }
      if (trim.isEmpty) Right(zeroes)
      else
        Right(
          zeroes ++ ByteVector(
            decoded
              .toByteArray
              .dropWhile(_ == 0)
          )
        ) //drop because toByteArray sometimes prepends a zero
    } catch {
      case e: IllegalArgumentException => Left(e.getMessage)
    }
  }
}
