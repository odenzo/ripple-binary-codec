package com.odenzo.ripple.bincodec.codecs

import scodec.bits.{Bases, ByteVector}
import spire.math.UByte

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.reference.RippleConstants

class RippleBase58ScodecTest extends OTestSpec {

  test("Single Digit") {
    RippleConstants.base58RippleAlphabet.foreach { c =>
      val answer = RippleBase58.fromBase58Descriptive(c.toString, RippleBase58Alphabet)
      answer.foreach(bv => scribe.debug(s"Str: $c -> ${bv.toHex}"))
    }
  }

  test("Web Thingt") {

    val answer = RippleBase58.fromBase58Descriptive("rJrRMgiRgrU6hDF4pgu5DXQdWyPbY35ErN", RippleBase58Alphabet) match {
      case Right(ans) =>
        val i = ans.toArray.map(x => UByte(x))
        scribe.debug(s"Ints: " + i.mkString("\n"))
    }

  }

}
