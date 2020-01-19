package com.odenzo.ripple.bincodec.codecs

trait ScodecStyleCodecs {

  import cats._
  import cats.data._
  import cats.implicits._
  import scodec.codecs._
  import spire.math.UByte
  import scodec._
  import scodec.bits._

  import com.odenzo.scodec.spire._
  import com.odenzo.ripple.bincodec.BCLibErr
  import com.odenzo.ripple.bincodec.BinCodecLibError

  private def adaptScodec(att: Attempt[BitVector]) = {
    att.toEither
      .leftMap(err => BinCodecLibError(err.messageWithContext))
      .map(_.bytes)
  }
  // All the numerics goto bit vectors by default, others go to ByteVector
  def scodecUByte(v: UByte): Either[BCLibErr, ByteVector] = adaptScodec(subyte.encode(v))

  def scodecUInt8(v: Long): Either[BCLibErr, ByteVector]  = (uint8.encode andThen adaptScodec)(v.toInt)
  def scodecUInt16(v: Long): Either[BCLibErr, ByteVector] = (uint16.encode andThen adaptScodec)(v.toInt)
  def scodecUInt32(v: Long): Either[BCLibErr, ByteVector] = (uint32.encode andThen adaptScodec)(v.toInt)

  def scodecUInt64(vs: BigInt): Either[BCLibErr, ByteVector] = {
    import spire.math.ULong
    // Canot use Long because ULong is out of range. From Javascript this is always a string Base10
    // Actually not sure this is used, as Amounts and other things in Strings are encoded differently
    if (vs < ULong.MaxValue && vs > 0) BinCodecLibError(s"$vs was in range 0...${ULong.MaxValue}").asLeft
    else suint64.encode(ULong.fromBigInt(vs)).toEither.map(_.bytes).leftMap(err => BinCodecLibError(err.messageWithContext))
  }

  def encodeVL(lengthToEnc: Int): Either[BCLibErr, ByteVector] = {
    val bytes = lengthToEnc match {
      // Is this really inclusive 192 =
      case l if Range(0, 192).inclusive.contains(l)     => UByte(l) :: Nil
      case l if Range(193, 12480).inclusive.contains(l) =>
        // 1100_0000 marks 2 bytes
        // 193 =>  HighByte: "0010" "xxxx" lowByte Anyway
        val l2: Int = l - 193
        UByte(193 + (l2 >>> 8)) :: UByte(l2) :: Nil

      case l if Range(12481, 918744).inclusive.contains(l) =>
        // First/High Nibble is "0000" "1101" 241 = 1111 0001 241 + 13
        // So its encoded 1111 on top we know its 3 bytes

        val length: Int = l - 12481 // Always fits in 5 nibbles.
        // This is almost certainly wrong and assumed length is Int (or Long?)
        List(
          UByte(241 + (length >>> 16)), // Munged ... but we could just take the bottom 2 bytes from length verbatim
          UByte((length >> 8) & 0xff),  // Middle Byte
          UByte(length & 0xff)          // Bottom Byte
        )

      case l => throw BinCodecLibError(s"Length $l was not in range 1..918744 for EncodeVL Length")
    }

    bytes.traverse(scodecUByte).map(bitl => bitl.reduce(_ ++ _))

  }

}

object ScodecStyleCodecs extends ScodecStyleCodecs
