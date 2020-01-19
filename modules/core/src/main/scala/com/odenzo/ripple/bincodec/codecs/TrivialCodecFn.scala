package com.odenzo.ripple.bincodec.codecs

import com.odenzo.ripple.bincodec.utils.JsonUtils

trait TrivialCodecFn extends JsonUtils {

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
  def encodeUByte(v: UByte): Either[BCLibErr, ByteVector] = adaptScodec(subyte.encode(v))
  def encodeUInt8(v: Long): Either[BCLibErr, ByteVector]  = (uint8.encode andThen adaptScodec)(v.toInt)
  def encodeUInt16(v: Long): Either[BCLibErr, ByteVector] = (uint16.encode andThen adaptScodec)(v.toInt)
  def encodeUInt32(v: Long): Either[BCLibErr, ByteVector] = (uint32.encode andThen adaptScodec)(v.toInt)

  def encodeUInt64(vs: BigInt): Either[BCLibErr, ByteVector] = {
    import spire.math.ULong
    // Canot use Long because ULong is out of range. From Javascript this is always a string Base10
    // Actually not sure this is used, as Amounts and other things in Strings are encoded differently
    if (vs > ULong.MaxValue || vs < 0) BinCodecLibError(s"$vs was not in range 0...${ULong.MaxValue}").asLeft
    else suint64.encode(ULong.fromBigInt(vs)).toEither.map(_.bytes).leftMap(err => BinCodecLibError(err.messageWithContext))
  }

  /**
    * When decoding, you can tell from the value of the first length byte whether there are 0, 1, or 2 additional length bytes:
    *
    * If the first length byte has a value of 192 or less, then that's the only length byte and it contains the exact length of the field contents in bytes.
    * If the first length byte has a value of 193 to 240, then there are two length bytes.
    * If the first length byte has a value of 241 to 254, then there are three length bytes.
    *
    * If the field contains 0 to 192 bytes of data, the first byte defines the length of the contents, then that many bytes of data follow immediately after the length byte.
    *  If the field contains 193 to 12480 bytes of data, the first two bytes indicate the length of the field with the following formula:
    *    193 + ((byte1 - 193) * 256) + byte2
    *  If the field contains 12481 to 918744 bytes of data, the first three bytes indicate the length of the field with the following formula:
    *    12481 + ((byte1 - 241) * 65536) + (byte2 * 256) + byte3
    *
    * @param lengthToEnc
    *
    * @return
    */
  def encodeVL(lengthToEnc: Int): Either[BCLibErr, ByteVector] = {
    lengthToEnc match {

      case l if Range(0, 192).inclusive.contains(l)     => encodeUByte(UByte(l))
      case l if Range(192, 12480).inclusive.contains(l) =>
        // 1100_0000 marks 2 bytes
        // LOL , this was always broken
        encodeUInt16(l - 192).map { len =>
          import spire.math.UInt
          val top    = UInt(193) + (UInt((l - 193).toLong) >> 8)
          val bottom = UByte(l - 193)
          ByteVector(top.toByte, bottom.toByte)
        }

      case l if Range(12481, 918744).inclusive.contains(l) =>
        // First/High Nibble is "0000" "1101" 241 = 1111 0001 241 + 13
        // So its encoded 1111 on top we know its 3 bytes
        // 12481 + ((byte1 - 241) * 65536) + (byte2 * 256) + byte3 = len (such a nice way of describing)
        encodeUInt32(l - 12481).map { len =>
          hex"F0" ++ (len >>> 16) ++ (len >> 8) & hex"ff" ++ (len & hex"0xff")
        }

      case l => BinCodecLibError(s"Length $l was not in range 0..918744 for EncodeVL Length").asLeft
    }

  }

  import io.circe.Json
  import scodec.bits.ByteVector

  import com.odenzo.ripple.bincodec.BCLibErr
  import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr

  /** Encodes the hex including the Variable Length info
    * This string must not be zero length string, or we maybe return a EmptyVal is it is.
    * */
  def encodeBlob(hex: String): ErrorOr[ByteVector] = {
    encodeHex(hex).flatMap(VLEncoding.prependVL)
  }

  /** Hex can have spaces and _ and optionally 0x in front, case ignored but migrate to uppercase as standard. */
  def encodeHex(hex: String) = {
    import scodec.bits.Bases.Alphabets
    import scodec.bits.ByteVector
    ByteVector.fromHexDescriptive(hex.toUpperCase, Alphabets.HexUppercase).leftMap(BinCodecLibError(_))

  }

  def encodeHash(hex: String, byteLen: Int): Either[BCLibErr, ByteVector] = {
    encodeHex(hex).ensure(BinCodecLibError(s"Hash was not length $byteLen"))(_.size === byteLen)
  }

  def encodeHash160(hex: String): Either[BCLibErr, ByteVector] = encodeHash(hex, 20)

  def encodeHash256(hex: String): Either[BCLibErr, ByteVector] = encodeHash(hex, 32)

  def encodeVector256(h256: List[String]): Either[BCLibErr, ByteVector] = {
    // Pendantic as we know the length for each really.
    val payload: Either[BCLibErr, List[ByteVector]] =
      for {
        payload <- h256.traverse(encodeHash256)
        totalLen = payload.map(_.length).sum
        vl <- VLEncoding.encodeVL(totalLen.toInt)
      } yield (vl :: payload)

    payload.map(_.reduce(_ ++ _))
  }
}

object TrivialCodecFn extends TrivialCodecFn
