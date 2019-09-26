package com.odenzo.ripple.bincodec.utils

import java.math.BigInteger
import scala.annotation.tailrec
import scala.util.Try

import cats.implicits._
import scribe.Logging

import com.odenzo.ripple.bincodec.BinCodecLibError

/** Based on
   https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
    This is being used by Ripple Signing instead of having multiple versions.
  */
object RippleBase58 extends Logging {

  val alphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
  val B58_ZERO = 'r'
  // char -> value
  val base58Map: Map[Char, BigInt] = alphabet.zipWithIndex.map {
    case (c, indx) => (c, BigInt(indx))
  }.toMap

  /**
    * This should never fail
    *
    * @param input binary data
    *
    * @return the base-58 representation of input, 0 bytes prefixes are converted to 'r'
    */
  def encode(input: Seq[Byte]): String = {
    if (input.isEmpty) ""
    else {
      val big     = new BigInteger(1, input.toArray)
      val builder = new StringBuilder

      @tailrec
      def encode1(current: BigInteger): Unit = current match {
        case BigInteger.ZERO => ()
        case _ =>
          val res: Array[BigInteger] = current.divideAndRemainder(BigInteger.valueOf(58L))
          val x                      = res(0)
          val remainder              = res(1)
          builder.append(alphabet.charAt(remainder.intValue))
          encode1(x)
      }

      encode1(big)
      // Same as BTC mapping 0x00 => "1"
      input.takeWhile(_ === 0).map(_ => builder.append(B58_ZERO))
      builder.toString().reverse
    }
  }

  /**
    * This potentially fails if String has char not in the alphabet.
    *
    *
    * @param input base-58 encoded data
    *
    * @return the decoded data in binary form. Leading r turns into zero pad
    */
  def decode(input: String): Either[BinCodecLibError, Seq[Byte]] = {
    BinCodecLibError.handling(s"Decoding B58 $input") {
      val zeroes: Seq[Byte] = input.takeWhile(c => c === 'r').iterator.map(_ => 0.toByte).toSeq
      val trim: String      = input.dropWhile(c => c === 'r')

      val base           = BigInt(58L)
      val zeroBI: BigInt = BigInt(0)
      trim match {
        case str if str.isEmpty => zeroes
        case str =>
          val decoded: BigInt = trim.foldLeft(zeroBI)((a, b) => (a * base) + base58Map(b))
          zeroes ++ decoded.toByteArray.dropWhile(_ === 0) // BigInteger.toByteArray may add a leading 0x00
      }
    }
  }
}
