package com.odenzo.ripple.bincodec.codecs

import scodec.codecs
import scodec.bits._
import cats._
import cats.data._
import cats.implicits._

trait ScodecStyleCodecs {

  import scodec.Attempt
  import scodec.codecs
  import scodec.codecs._
  import spire.math.UByte

  import com.odenzo.ripple.bincodec.BCLibErr
  import com.odenzo.ripple.bincodec.BinCodecLibError
  import com.odenzo.ripple.bincodec.RawValue

  // These may migrate to scodec.Codec types soon...

  def scodecUInt8(v: Long): Attempt[BitVector] = uint8.encode(v.toInt)

  def scodecUInt16(v: Long): Attempt[BitVector] = uint16.encode(v)

  def scodecUInt32(v: Long): Attempt[BitVector] = uint32.encode(v)

  def scodecUInt64(vs: BigInt): Attempt[BitVector] = {
    import scodec.Attempt
    import scodec.Err
    import scodec.bits.BitVector
    import scodec.bits.ByteVector
    import spire.math.ULong
    // Canot use Long because ULong is out of range. From Javascript this is always a string Base10
    // Actually not sure this is used, as Amounts and other things in Strings are encoded differently
    if (vs < ULong.MaxValue && vs > 0) {
      val bits = scodec.Codec[ULong].encode(ULong.fromBigInt(vs))
      bits
    } else {
      Attempt.failure[BitVector](Err(s"$vs was in range 0...${ULong.MaxValue}"))
    }
  }

  def encodeVL(lengthToEnc: Int): Either[BCLibErr, ByteVector] = {
    lengthToEnc match {
      // Is this really inclusive 192 =
      case l if Range(0, 192).inclusive.contains(l)     => ByteVector.fromInt(l, size = 8).asRight
      case l if Range(193, 12480).inclusive.contains(l) =>
        // 1100_0000 marks 2 bytes
        // 193 =>  HighByte: "0010" "xxxx"          lowByte Anyway
        val l2: Int  = l - 193
        val lowByte  = uint8.encode(l2).require.bytes
        val highByte = uint8.encode(UByte(193 + (l2 >>> 8)).toInt).require.bytes
        (highByte ++ lowByte).asRight

      case l if Range(12481, 918744).inclusive.contains(l) =>
        // First/High Nibble is "0000" "1101"     241 = 1111 0001       241 + 13
        // So its encoded 1111 on top we know its 3 bytes

        import scodec.Codec
        val length = l - 12481 // Always fits in 5 nibbles.
        scodec
          .Codec[List[UByte]]
          .encode(
            List(
              UByte(241 + (length >>> 16)), // Munged ... but we could just take the bottom 2 bytes from length verbatim
              UByte((length >> 8) & 0xff),  // Middle Byte
              UByte(length & 0xff)          // Bottom Byte
            )
          )
          .require
          .bytes
          .asRight

      case l => BinCodecLibError(s"Length $l was not in range 1..918744 for EncodeVL Length").asLeft
    }

  }

}

object ScodecStyleCodecs extends ScodecStyleCodecs
