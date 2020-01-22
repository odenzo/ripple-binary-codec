package com.odenzo.ripple.bincodec.scodecs

import scodec.bits.Bases.{Alphabet, Alphabets}
import scodec.bits.{Bases, ByteVector}

/** This is not happy... go back to manual or investigate. Maybe it is the dreaded r */
object RippleBase58Alphabet extends Alphabet {

  import com.odenzo.ripple.bincodec.reference.RippleConstants

  val base58RippleAlphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
  val rippleAlphabet       = Alphabets.Base58

  private val Chars = base58RippleAlphabet.toIndexedSeq

  def toChar(i: Int) = Chars(i)

  def toIndex(c: Char) = Chars.indexOf(c) match {
    case -1 => throw new IllegalArgumentException
    case i  => i

  }

  def ignore(c: Char) = c.isWhitespace
}
