package com.odenzo.ripple.bincodec.codecs

import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.BinCodecLibError
import cats._
import cats.data._
import cats.implicits._
import scribe.Logging

object IssuedAmountScratch extends ByteUtils with JsonUtils with Logging {

  /** Breaks down to UBytes for the amount, currency amd issuer */
  def breakFiat(hex: String): Either[BinCodecLibError, (List[UByte], List[UByte], List[UByte])] = {

    val all: Either[BinCodecLibError, List[UByte]] = ByteUtils.hex2ubytes(hex)
    val amount                                     = all.map(_.take(8)) // Top 64 is amount in sign and flag
    val currency                                   = all.map(_.slice(8, 28)) // 160 bits
    val issuer                                     = all.map(_.slice(32, 52)) // another 160 bits
    (amount, currency, issuer).mapN((_, _, _))
  }

  /** Get Top 2 bits, Exponent (Shifted) anf the mantissa in that order in a list.
    * Moved down so the 2 bits in ULong has value 3 is both set etc.
    * */
  def breakFiatAmount(fields: ULong): List[ULong] = {

    // We char about the first 10 bits contains in the first two bytes

    val topMask: ULong      = ULong(0xC000000000000000L)
    val expMask: ULong      = ULong(0xFF) << 54
    val mantissaMask: ULong = ULong(0x3FFFFFFFFFFFFFL) // 13 nibbles

    //    scribe.debug("Masks:\n" + ByteUtils.uLong2Base2Str(topMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(expMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(mantissaMask))

    val top2     = (fields & topMask) >> 62
    val exp      = (fields & expMask) >> 54
    val mantissa = fields & mantissaMask

    List(top2, exp, mantissa)
  }
}
