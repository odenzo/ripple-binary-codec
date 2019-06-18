package com.odenzo.ripple.bincodec.codecs

import scala.util.Try

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.{Decoder, Json}
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.{EncodedDataType, RawValue}
import com.odenzo.ripple.bincodec.encoding.TypeSerializers.{dd, encodeULong}
import com.odenzo.ripple.bincodec.reference.RippleDataType
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, OErrorRipple, RippleCodecError}

/** Deals with Blobs and Hashes and things that are plain hex encoded in Json */
trait HashHexCodecs extends StrictLogging {


  def encodeHash(json: Json, byteLen: Int): Either[RippleCodecError, RawValue] = {
    // This looks like Hex Already... in fact just round tripping
    val str = JsonUtils.decode(json, Decoder[String])
    val ans: Either[RippleCodecError, List[UByte]] = str.flatMap{ v ⇒
      ByteUtils.hex2ubytes(v)
    }
    val checked = ans.flatMap(ByteUtils.ensureMaxLength(_, byteLen))
    checked.map(ByteUtils.zeroPadBytes(_, byteLen))
    ans.fmap(RawValue)
  }

  def encodeHash160(json: Json): Either[RippleCodecError, EncodedDataType] = {
    val rtype: Either[OErrorRipple, RippleDataType] = dd.getTypeObj("Hash160")
    val encoded: Either[RippleCodecError, RawValue] = encodeHash(json, 20)

    (encoded, rtype).mapN(EncodedDataType)
  }

  def encodeHash256(json: Json): Either[RippleCodecError, EncodedDataType] = {
    val rtype: Either[OErrorRipple, RippleDataType] = dd.getTypeObj("Hash256")
    val encoded: Either[RippleCodecError, RawValue] = encodeHash(json, 32)

    (encoded, rtype).mapN(EncodedDataType(_, _))
  }

}



object HashHexCodecs extends HashHexCodecs
