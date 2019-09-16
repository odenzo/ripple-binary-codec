package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import spire.math.UByte

import com.odenzo.ripple.bincodec.{EncodedVL, BCLibErr, RawValue, BinCodecLibError}

trait VLEncoding {

  /** Prepend Variable Length encoding the the list of Bytes.
    * Special case is empty list, which returns encoded with length 0 and empty rawvalue
    * @param bytes
    * @return
    */
  def prependVL(bytes: List[UByte]): Either[BCLibErr, EncodedVL] = {
    encodeVL(bytes.length).map(v => EncodedVL(v, RawValue(bytes)))
  }

  /** We are going to encode the len in an array of bytes betwen 0 and 4 bytes long.
    * UInt8 array is used in Javascript. Length is unsigned of course
    * This is not EncodedValue because a sub-value
    *  Multisigning sends in SigningPubKey which is VLEncoded Blob but empty, so cannot be VLEncoded.
    * @param lengthToEnc
    *
    * @return The encodedVL, which 1, 2, or 3 bytes. There is no RiplpleType for this
    */
  def encodeVL(lengthToEnc: Int): Either[BCLibErr, RawValue] = {
    val vl = lengthToEnc match {

      // Is this really inclusive 192 = 11000000
      case l if Range(0, 192).inclusive.contains(l) => (UByte(l) :: Nil).asRight
      case l if Range(193, 12480).inclusive.contains(l) =>
        val l2: Int = l - 193
        List(UByte(193 + (l2 >>> 8)), UByte(l2 & 0xff)).asRight
      case l if Range(12481, 918744).inclusive.contains(l) =>
        val length = l - 12481
        List(
          UByte(241 + (length >>> 16)),
          UByte((length >> 8) & 0xff),
          UByte(length & 0xff)
        ).asRight

      case l => BinCodecLibError(s"Length $l was not in range 1..918744 for EncodeVL Length").asLeft
    }

    vl.map(RawValue.apply)
  }

  /** Note that data may have more fields after the VL encoded one */
  def decodeVL(data: List[UByte]): Either[BCLibErr, (Int, List[UByte])] = {
    // If top two bits are zero its one byte
    // This just calculates the number of bytes in VL
    // FIXME: Isn't actaully returning the length yet, except < 192 lengths
    // The length is data.length - 1 , 2 or 3 cheating.
    // Better to do the full decode
    val left: Either[BCLibErr, (Int, List[UByte])] = data match {
      case h :: t if h <= UByte(192) => (h.toInt, data.drop(1)).asRight
      case h :: t if h <= UByte(240) => (0, data.drop(2)).asRight
      case h :: t if h <= UByte(254) => (0.toInt, data.drop(3)).asRight
      case other                     => BinCodecLibError(s"Illegal VL Encoding").asLeft
    }

    left
  }
}

object VLEncoding extends VLEncoding
