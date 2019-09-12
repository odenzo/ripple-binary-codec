package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Json, Decoder}
import spire.math.UByte

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.reference.RippleDataType
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{EncodedDataType, BCLibErr, RawValue, BinCodecLibError}

/** Deals with Blobs and Hashes and things that are plain hex encoded in Json */
trait HashHexCodecs extends CodecUtils {

  def encodeHash(json: Json, byteLen: Int): Either[BinCodecLibError, RawValue] = {
    // This looks like Hex Already... in fact just round tripping
    val str = JsonUtils.decode(json, Decoder[String])
    val ans: Either[BinCodecLibError, List[UByte]] = str.flatMap { v =>
      ByteUtils.hex2ubytes(v)
    }
    val checked = ans.flatMap(ByteUtils.ensureMaxLength(_, byteLen))
    checked.map(ByteUtils.zeroPadBytes(_, byteLen))
    ans.fmap(RawValue)
  }

  def encodeHash160(json: Json): Either[BinCodecLibError, EncodedDataType] = {
    val rtype: Either[BCLibErr, RippleDataType]     = dd.getTypeObj("Hash160")
    val encoded: Either[BinCodecLibError, RawValue] = encodeHash(json, 20)

    (encoded, rtype).mapN(EncodedDataType)
  }

  def encodeHash256(json: Json): Either[BinCodecLibError, EncodedDataType] = {
    val rtype: Either[BCLibErr, RippleDataType]     = dd.getTypeObj("Hash256")
    val encoded: Either[BinCodecLibError, RawValue] = encodeHash(json, 32)

    (encoded, rtype).mapN(EncodedDataType(_, _))
  }

}

object HashHexCodecs extends HashHexCodecs
