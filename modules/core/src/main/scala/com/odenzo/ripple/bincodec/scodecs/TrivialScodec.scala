package com.odenzo.ripple.bincodec.scodecs

import _root_.scodec.codecs._
import scodec.{Attempt, Codec, Err}
import scodec.bits._
import scodec.bits.Bases.Alphabets
import spire.math.{UInt, ULong}

import com.odenzo.scodec.spire._

object TrivialScodec {

  // s* stuff from Spire if needed, generally can work with BitVector and ByteVector now
  val spireBuilt: Codec[UInt] = suint8

  /** Encodes the hex including the Variable Length info
    * This string must not be zero length string, or we maybe return a EmptyVal is it is.
    * @todo xrpblob VLEncoding and Decoding */
  //@todo xrpblob VLEncoding and Decoding
  //val xrpblob = vl.flatZip() ~ ubyte()

  def hexStringToBits(hex: String): Attempt[BitVector] = {
    BitVector.fromHexDescriptive(hex.toUpperCase, Alphabets.HexUppercase) match {
      case Left(err) => Attempt.failure(Err(err))
      case Right(v)  => Attempt.successful(v)
    }
  }

  def xrphex(lenInNibbles: Int): Codec[String] = {
    bitsStrict(lenInNibbles * 4).exmap[String](bitsToHex, hexStringToBits)
  }
  // Might want to make sure the bits are divisiable by four, now padded
  def bitsToHex(bits: BitVector): Attempt[String] = { Attempt.successful(bits.toHex(Alphabets.HexUppercase)) }

  def xrphash(byteLen: Int): Codec[String] = xrphex(byteLen * 2)

  val xrphash160: Codec[String] = xrphash(20)

  val xrphash256: Codec[String] = xrphash(32)

  // This is VL Encoded (256/8 * n elems)
  // @todo VLEncoding of total length
  val xrpvectorhash256 = vector(xrphash256)

  val xrpuint8: Codec[Int]   = uint8
  val xrpuint16: Codec[Int]  = uint16
  val xrpuint32: Codec[Long] = uint32

  // 64 in progress and for testing. If max at 63 bits we are ok.
  // So, this should accept long in given range (63 bits) but output/input 64 bits
  val xrpulong64: Codec[ULong] = suint64

  ulong(63).econtramap[Long] { l: Long =>
    if (l <= ULong.MaxValue.toLong) Attempt.successful(l)
    else Attempt.failure(Err(s"$l was too big for ULong"))
  }

  def bitv64ToULong(bv: BitVector) = {
    // There is no bytes to ULong so we must go via BigInt or a String

    uint32.decode(bv.takeRight(16))
    //suint64.decode()
    // bv.take(16) then convert to BigInt
  }

  def stringToBigInt(str: String): Attempt[BigInt] = {
    try {
      Attempt.successful(BigInt(str))
    } catch {
      case e: Exception => Attempt.failure(Err(s"$str wasnt valid BigInt ${e.getLocalizedMessage}"))
    }
  }

  def bigint2ulong(bi: BigInt): Attempt[BigInt] = {
    val ul = ULong.fromBigInt(bi)
    if (ul.isValidLong) Attempt.successful(ul)
    else Attempt.failure(Err(s"Not a valid long, but maybe ulong in range $ul"))
  }

}
