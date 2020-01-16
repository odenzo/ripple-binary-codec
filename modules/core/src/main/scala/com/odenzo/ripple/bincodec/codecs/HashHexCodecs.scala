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

import scodec.Attempt
import scodec.bits.BitVector
import scodec.Codec

import com.odenzo.scodec.spire
import scodec.bits
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._

/** Deals with Blobs and Hashes and things that are plain hex encoded in Json */
trait HashHexCodecs extends CodecUtils {

  import cats._
  import cats.data._
  import cats.implicits._

  import cats.effect._

  def encodeHash(hex: String, byteLen: Int): Either[BCLibErr, ByteVector] = {
    MiscCodecs
      .encodeHex(hex)
      .ensure(BinCodecLibError(s"Hash was not length $byteLen"))(_.size === byteLen)
  }

  def encodeHash160(hex: String): Either[BCLibErr, ByteVector] = encodeHash(hex, 20)
  def encodeHash256(hex: String): Either[BCLibErr, ByteVector] = encodeHash(hex, 32)

}

object HashHexCodecs extends HashHexCodecs
