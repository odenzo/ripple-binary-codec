package com.odenzo.ripple.bincodec.serializing

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.UByte

import com.odenzo.ripple.bincodec.serializing.BinarySerializer.RawEncodedValue
import com.odenzo.ripple.bincodec.utils.caterrors.{CodecError, OError}

trait VLEncoding extends StrictLogging {


  def prependVL(bytes: List[UByte]): Either[OError, RawEncodedValue] = {
    val vl: Either[OError, RawEncodedValue] = encodeVL(bytes.length)
    val concatBytes = vl.map(rev ⇒ rev.ubytes ++ bytes)
    concatBytes.map(RawEncodedValue)
  }

  /** We are going to encode the len in an array of bytes betwen 0 and 4 bytes long.
    * UInt8 array is used in Javascript. Length is unsigned of course
    * This is not EncodedValue because a sub-value
    *
    * @param lengthToEnc
    *
    * @return The encodedVL, which 1, 2, or 3 bytes. There is no RiplpleType for this
    */
  def encodeVL(lengthToEnc: Int): Either[OError, RawEncodedValue] = {
    val vl = lengthToEnc match {

      case l if Range(1, 192).inclusive.contains(l)        => (UByte(l) :: Nil).asRight
      case l if Range(193, 12480).inclusive.contains(l)    ⇒
        val l2: Int = l - 193
        List(UByte(193 + (l2 >>> 8)), UByte(l2 & 0xff)).asRight
      case l if Range(12481, 918744).inclusive.contains(l) ⇒
        val length = l - 12481
        List(
              UByte(241 + (length >>> 16)),
              UByte((length >> 8) & 0xff),
              UByte(length & 0xff),
              ).asRight

      case l => CodecError(s"Length $l was not in range 1..918744 for EncodeVL Length").asLeft
    }

    vl.map(RawEncodedValue)
  }

}

object VLEncoding extends VLEncoding
