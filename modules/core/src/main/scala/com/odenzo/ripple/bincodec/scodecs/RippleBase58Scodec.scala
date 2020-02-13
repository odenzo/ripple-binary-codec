package com.odenzo.ripple.bincodec.scodecs

import scala.annotation.tailrec
import scala.util.Try

import scodec.bits.{Bases, BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err}

trait RippleBase58Scodec {

  /** Eager RippleBase58 scodec that will consume all the bytes */
  val xrplBase58 = Codec(fromBase58(_, RippleBase58Alphabet), toBase58(_, RippleBase58Alphabet))

  final def toRippleBase58(vector: BitVector): Attempt[String] = {
    toBase58(vector, RippleBase58Alphabet).map(_.value) // Throw away remaining as it consumes all
  }

  final def toBase58(bitsv: BitVector, alphabet: Bases.Alphabet = RippleBase58Alphabet): Attempt[DecodeResult[String]] = {
    val bv = bitsv.bytes
    Attempt.fromTry {
      Try {
        val zeroChar = alphabet.toChar(0)
        if (bv.isEmpty) {
          DecodeResult("", BitVector.empty)
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

          val result = go(BigInt(1, bv.toArray), List.empty)
          DecodeResult(result, BitVector.empty)

        }
      }
    }

  }

  private def fromBase58(str: String, alphabet: Bases.Alphabet = RippleBase58Alphabet): Attempt[BitVector] = {
    val zeroChar   = alphabet.toChar(0)
    val zeroLength = str.takeWhile(_ == zeroChar).length
    val zeroes     = ByteVector.fill(zeroLength.toLong)(0)
    val trim       = str.splitAt(zeroLength)._2.toList
    val RADIX      = BigInt(58L)
    Attempt.fromTry {
      Try {
        val decoded = trim.foldLeft(BigInt(0)) { (a, c) =>
          try {
            a * RADIX + BigInt(alphabet.toIndex(c))
          } catch {
            case e: IllegalArgumentException =>
              val idx = trim.takeWhile(_ != c).length
              throw new IllegalArgumentException(s"Invalid base 58 character '$c' at index $idx")
          }
        }
        if (trim.isEmpty) zeroes.bits
        else
          (zeroes ++ ByteVector(decoded.toByteArray.dropWhile(_ == 0))).bits
        //drop because toByteArray sometimes prepends a zero
      }
    }

  }
}

object RippleBase58Scodec extends RippleBase58Scodec
