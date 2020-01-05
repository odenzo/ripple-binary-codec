package com.odenzo.ripple.bincodec.codecs

import scodec.codecs
import scodec.bits._
import cats._
import cats.data._
import cats.implicits._

class ScodecStyleCodecs {

  import scodec.codecs
  import scodec.codecs._
  import spire.math.UByte

  import com.odenzo.ripple.bincodec.BCLibErr
  import com.odenzo.ripple.bincodec.BinCodecLibError
  import com.odenzo.ripple.bincodec.RawValue

  def scodecUInt8(v: Int) = uint8.encode(v)

  def scodecUInt16(v: Int) = uint16.encode(v)

  def scodecUInt32(v: Long) = uint32.encode(v)

  def scodecUInt64(vs: String) = {
    import scodec.Attempt
    import scodec.Err
    import scodec.bits.BitVector
    import scodec.bits.ByteVector
    import spire.math.ULong
    // Canot use Long because ULong is out of range. From Javascript this is always a string Base10
    // Actually not sure this is used, as Amounts and other things in Strings are encoded differently
    BigInt(vs)
    val ulong                   = ULong.apply(vs)
    val hex                     = ulong.toString(16)
    val res: Attempt[BitVector] = Attempt.Failure(Err("Not Implemented UInt64 Yet"))
    res
  }

  def encodeVL(lengthToEnc: Int): Either[BCLibErr, RawValue] = {
    val vl = lengthToEnc match {

      // Is this really inclusive 192 = 11000000
      case l if Range(0, 192).inclusive.contains(l) => ByteVector.fromInt(l, size = 8).asRight
      case l if Range(193, 12480).inclusive.contains(l) =>
        val l2: Int = l - 193
        for {
          lowByte <- uint8.encode(l2).map(_.toByteVector) // Just take bottom 8 bits
          highByte = UByte(193 + (l2 >>> 8))
          high <- ubyte(8).encode(highByte.byteValue()).map(_.toByteVector)
        } yield (high ++ lowByte)

      case l if Range(12481, 918744).inclusive.contains(l) =>
        val length = l - 12481
        ByteVector(
          List(
            UByte(241 + (length >>> 16)),
            UByte((length >> 8) & 0xff),
            UByte(length & 0xff)
          )
        ).asRight

      case l => BinCodecLibError(s"Length $l was not in range 1..918744 for EncodeVL Length").asLeft
    }

    vl.map(RawValue.apply)
  }

}
