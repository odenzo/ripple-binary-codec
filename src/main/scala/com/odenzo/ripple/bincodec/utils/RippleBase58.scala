package com.odenzo.ripple.bincodec.utils

import java.math.BigInteger
import scala.annotation.tailrec

import com.typesafe.scalalogging.StrictLogging
import spire.math.UByte

/** Based on
  https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/Base58.scala
  Note: This is restricted to bincodec.* packages
*/
private[bincodec] object RippleBase58 extends StrictLogging {

  
  val std = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val alphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  // char -> value
  val base28Map: Map[Char, Int] = alphabet.zipWithIndex.toMap

  /**
    * This should never fail
    * @param input binary data
    *
    * @return the base-58 representation of input
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
          val Array(x, remainder) = current.divideAndRemainder(BigInteger.valueOf(58L))
          builder.append(alphabet.charAt(remainder.intValue))
          encode1(x)
      }

      encode1(big)
      input.takeWhile(_ == 0).map(_ => builder.append(alphabet.charAt(0)))
      builder.toString().reverse
    }
  }

  /**
    * This potentially fails if String has char not in the alphabet.
    * @param input base-58 encoded data
    *
    * @return the decoded data
    */
  def decode(input: String): Array[Byte] = {
    val zeroes = input.takeWhile(_ == '1').map(_ => 0: Byte).toArray
    val trim   = input.dropWhile(_ == '1').toList
    val decoded = trim
      .foldLeft(BigInteger.ZERO)(
        (a, b) =>
          a.multiply(BigInteger.valueOf(58L))
            .add(BigInteger.valueOf(base28Map(b).toLong))
      )
    if (trim.isEmpty) zeroes
    else
      zeroes ++ decoded.toByteArray
        .dropWhile(_ == 0) // BigInteger.toByteArray may add a leading 0x00
  }


  /** This handles Base28 Keys, like Seed and Public Key.
    * It does the normal decoding and drops the first marker byte and the last four checksum bytes (?)
    *
    * @param b58
    */
  def base58Key2bytesTrimmed(b58: String): List[UByte] = {
    // No Combination here gives us correct answer
   val asBytes: List[UByte] = base58Key2bytes(b58)
   logger.info(s"As Bytes ${ByteUtils.ubytes2hex(asBytes)}  len= ${asBytes.length}")
    asBytes.drop(1)     // The first marker 33, this is used to determine key type and part of checksum
    .dropRight(4)       // The Checksum which we don't use

  }

  def base58Key2bytes(b28: String): List[UByte] = {
    RippleBase58.decode(b28).map(ByteUtils.byte2ubyte).toList
  }

  def base58ToHex(b28: String): String = {
    ByteUtils.ubytes2hex(base58Key2bytes(b28))
  }
}
