package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import spire.math.UByte

import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.{EncodedVL, RawValue}

trait VLEncoding  {

  def prependVL(bytes: List[UByte]): Either[OErrorRipple, EncodedVL] = {
    encodeVL(bytes.length).map(v ⇒ EncodedVL(v, RawValue(bytes)))

  }

  /** We are going to encode the len in an array of bytes betwen 0 and 4 bytes long.
    * UInt8 array is used in Javascript. Length is unsigned of course
    * This is not EncodedValue because a sub-value
    *
    * @param lengthToEnc
    *
    * @return The encodedVL, which 1, 2, or 3 bytes. There is no RiplpleType for this
    */
  def encodeVL(lengthToEnc: Int): Either[OErrorRipple, RawValue] = {
    val vl = lengthToEnc match {

      // Is this really inclusive 192 = 11000000
      case l if Range(1, 192).inclusive.contains(l) => (UByte(l) :: Nil).asRight
      case l if Range(193, 12480).inclusive.contains(l) ⇒
        val l2: Int = l - 193
        List(UByte(193 + (l2 >>> 8)), UByte(l2 & 0xff)).asRight
      case l if Range(12481, 918744).inclusive.contains(l) ⇒
        val length = l - 12481
        List(
          UByte(241 + (length >>> 16)),
          UByte((length >> 8) & 0xff),
          UByte(length & 0xff),
        ).asRight

      case l => RippleCodecError(s"Length $l was not in range 1..918744 for EncodeVL Length").asLeft
    }

    vl.map(RawValue)
  }

  def decodeVL(data: List[UByte]): Either[OErrorRipple, (Int, List[UByte])] = {
    // If top two bits are zero its one byte
    // This just calculates the number of bytes in VL
    // FIXME: Isn't actaully returning the length yet, except < 192 lengths
    val headInt: Int = data.head.toInt

    val oneByte = Range(0,192).inclusive
    val twoByte = Range(193,240).inclusive
    val threeByte = Range(241,254).inclusive

    
    if (headInt <= 192) {
      (headInt, data.drop(1)).asRight
    } else if (Range(193, 240).inclusive.contains(headInt)) {
      (0, data.drop(2)).asRight
    } else if (Range(241, 254).inclusive.contains(headInt)) {
      (0.toInt, data.drop(3)).asRight
    } else {
      RippleCodecError(s"Illegal VL Encoding").asLeft
    }

  }
}

object VLEncoding extends VLEncoding
