package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import spire.math.UByte

import com.odenzo.ripple.bincodec.BCLibErr
import com.odenzo.ripple.bincodec.BinCodecLibError

trait VLEncoding {

  import scodec.Codec
  import scodec.bits._

  /** Prepend Variable Length encoding the the list of Bytes.
    * Special case is empty list, which returns encoded with length 0 and empty rawvalue
    *
    * @param bytes
    *
    * @return
    */
  def prependVL(bytes: ByteVector): Either[BCLibErr, ByteVector] = {
    encodeVL(bytes.length.toInt).map(_ ++ bytes)
  }

  def encodeVL(len: Int): Either[BCLibErr, ByteVector] = {
    import ScodecStyleCodecs._

    Either
      .cond(len >= 0 && len <= 918744, len, BinCodecLibError(s"Length $len was not in range 1..918744 for EncodeVL Length"))
      .map {
        // Is this really inclusive 192 = 11000000
        case l if Range(0, 192).inclusive.contains(l) =>
          // Top 2 bits not set of n1, return nib1,nib2
          List(UByte(l))

        case l if Range(193, 12480).inclusive.contains(l) =>
          // Bottom Two bits of n1 from nib1, nib2, nib3, nib4
          val l2: Int = l - 193
          List(UByte(193 + (l2 >>> 8)), UByte(l2 & 0xff))

        case l if Range(12481, 918744).inclusive.contains(l) =>
          val length = l - 12481

          List(
            UByte(241 + (length >>> 16)),
            UByte((length >> 8) & 0xff),
            UByte(length & 0xff)
          )

      }
      .flatMap(bl => bl.traverse(scodecUByte))
      .map(bvl => bvl.reduce(_ ++ _))
  }

//  /**  This just calculates the number of bytes in VL and consumes. Don't rely on answer */
//  def decodeVL(data: List[UByte]): Either[BCLibErr, (Int, List[UByte])] = {
//    // If top two bits are zero its one byte
//
//    // FIXME: Isn't actaully returning the length yet, except < 192 lengths
//    // The length is data.length - 1 , 2 or 3 cheating.
//    // Better to do the full decode
//    val left: Either[BCLibErr, (Int, List[UByte])] = data match {
//      case h :: t if h <= UByte(192) => (h.toInt, data.drop(1)).asRight
//      case h :: t if h <= UByte(240) => (0, data.drop(2)).asRight
//      case h :: t if h <= UByte(254) => (0.toInt, data.drop(3)).asRight
//      case other                     => BinCodecLibError(s"Illegal VL Encoding").asLeft
//    }
//
//    left
//  }
}

object VLEncoding extends VLEncoding
