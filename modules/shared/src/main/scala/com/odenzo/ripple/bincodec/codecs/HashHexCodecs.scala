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

    for {
      str <- JsonUtils.decode(json, Decoder[String])
      ans <- ByteUtils.hex2ubytes(str)
      _   <- ByteUtils.ensureMaxLength(ans, byteLen)
    } yield RawValue(ans)
  }

  def encodeHash160(json: Json): Either[BinCodecLibError, EncodedDataType] = {
    for {
      rtype   <- dd.getTypeObj("Hash160")
      encoded <- encodeHash(json, 20)
    } yield EncodedDataType(encoded, rtype)
  }

  def encodeHash256(json: Json): Either[BinCodecLibError, EncodedDataType] = {
    for {
      rtype   <- dd.getTypeObj("Hash256")
      encoded <- encodeHash(json, 32)
    } yield EncodedDataType(encoded, rtype)
  }

}

object HashHexCodecs extends HashHexCodecs
