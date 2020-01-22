package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.codecs.{VLEncoding, VLEncodingOldBroken}
import com.odenzo.ripple.bincodec.{BCLibErr, BinCodecLibError}
import scodec.bits.ByteVector
import _root_.scodec.codecs._
import io.circe.Json
import scodec.Codec

import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr
import scodec.bits._
import scodec.bits.Bases.Alphabets
import spire.math.UInt

import com.odenzo.scodec.spire._

class TrivialScodec {

  // s* stuff from Spire if needed, generally can work with BitVector and ByteVector now
  val spireBuilt: Codec[UInt] = suint8

  /** Encodes the hex including the Variable Length info
    * This string must not be zero length string, or we maybe return a EmptyVal is it is.
    * @todo xrpblob VLEncoding and Decoding */
  //val xrpblob = vl.flatZip() ~ ubyte()

  /** size is # of hex chars, i.e.  sizeInBytes * 2 , uppercase */
  def xrphex(size: Int): Codec[String] = Codec(
    utf8.map(hex => ByteVector.fromHexDescriptive(hex.toUpperCase, Alphabets.HexUppercase)).asEncoder,
    bitsStrict(size * 4).map(_.toHex(Alphabets.HexUppercase)).asDecoder
  )

  def xrphash(byteLen: Int): Codec[String] = xrphex(byteLen * 2)

  val xrphash160: Codec[String] = xrphash(20)

  val xrphash256: Codec[String] = xrphash(32)

  // This is VL Encoded (256/8 * n elems)
  // @todo VLEncoding of total length
  val xrpvectorhash256 = vector(xrphash256)

  val xrpuint8  = uint8
  val xrpuint16 = uint16
  val xrpuint32 = uint32

  // 64 in progress and for testing. If max at 63 bits we are ok.
  val xrpuint64 = suint64

//  def encodeUInt64(vs: BigInt): Either[BCLibErr, ByteVector] = {
//    import spire.math.ULong
//    // Canot use Long because ULong is out of range. From Javascript this is always a string Base10
//    // Actually not sure this is used, as Amounts and other things in Strings are encoded differently
//    if (vs > ULong.MaxValue || vs < 0) BinCodecLibError(s"$vs was not in range 0...${ULong.MaxValue}").asLeft
//    else suint64.encode(ULong.fromBigInt(vs)).toEither.map(_.bytes).leftMap(err => BinCodecLibError(err.messageWithContext))
//  }

  /**
    * I forget what is inside this, always Hex 256 I think
    * VLEncoded
    */
  def encodeVector256(data: List[String]): Either[BinCodecLibError, ByteVector] = {
    xrpvl ~ xrpvectorhash256 // These may or may not be hashes but quack the same
  }

  val decodeVector256 = xrpvl.flatZip(blah blah)

  import scodec.bits.ByteVector

}
