package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.BCLibErr
import com.odenzo.ripple.bincodec.BinCodecLibError
import scodec.bits._

/** Deals with Blobs and Hashes and things that are plain hex encoded in Json */
trait HashHexCodecs extends CodecUtils {

  import cats._
  import cats.data._
  import cats.implicits._

  def encodeHash(hex: String, byteLen: Int): Either[BCLibErr, ByteVector] = {
    MiscCodecs
      .encodeHex(hex)
      .ensure(BinCodecLibError(s"Hash was not length $byteLen"))(_.size === byteLen)
  }

  def encodeHash160(hex: String): Either[BCLibErr, ByteVector] = encodeHash(hex, 20)
  def encodeHash256(hex: String): Either[BCLibErr, ByteVector] = encodeHash(hex, 32)

}

object HashHexCodecs extends HashHexCodecs
