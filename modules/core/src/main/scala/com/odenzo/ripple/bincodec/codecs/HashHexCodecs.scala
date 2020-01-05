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

  def encodeHash(json: Json, byteLen: Int): Either[BCLibErr, ByteVector] = {
    import scodec.bits.Bases.Alphabets
    // This looks like Hex Already... in fact just round tripping
    // throws IllegalArgumentException is not valie

    Either
      .fromOption(json.asString, s"JSON was not a String")
      .map(_.toUpperCase)
      .flatMap(txt => ByteVector.fromHexDescriptive(txt, Alphabets.HexUppercase))
      .ensure(s"Hash was not length $byteLen")(_.size === byteLen)
      .leftMap(BinCodecLibError(_))

  }

  def encodeHash160(json: Json): Either[BCLibErr, EncodedDataType] = {
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
