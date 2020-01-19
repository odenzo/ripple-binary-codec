package com.odenzo.ripple.bincodec.codecs

import scodec.bits.Bases.Alphabet

object RippleBase58 extends Alphabet {

  import com.odenzo.ripple.bincodec.reference.RippleConstants

  private val Chars = RippleConstants.base58RippleAlphabet.toIndexedSeq

  def toChar(i: Int) = Chars(i)

  def toIndex(c: Char) = Chars.indexOf(c) match {
    case -1 => throw new IllegalArgumentException
    case i  => i

  }

  def ignore(c: Char) = c.isWhitespace
}
