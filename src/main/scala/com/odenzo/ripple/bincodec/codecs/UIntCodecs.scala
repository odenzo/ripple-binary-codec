package com.odenzo.ripple.bincodec.codecs

import scala.util.Try

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.ULong

import com.odenzo.ripple.bincodec.RawValue
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, OErrorRipple, RippleCodecError}

trait UIntCodecs extends CodecUtils {

  /** Adapted from the Javascript
    * Redo with one UInt Decoder and a function for each type.
    * */
  def encodeUIntN(v: Json, dataType: String): Either[RippleCodecError, RawValue] = {
    BinCodecExeption.wrap(s"Binary Encoding JSON # $v"){

      val number: Option[ULong] = for {
        numeric <- v.asNumber
        ulong <- numeric.toLong.map(ULong(_))
      } yield ulong

      val unsigned = Either.fromOption(number, RippleCodecError(s"JSON ${v.spaces2} was not a Unisgned Long Number"))

      val ans: Either[OErrorRipple, RawValue] = unsigned.flatMap(v ⇒ encodeULong(v, dataType))
      ans
    }
  }

  def encodeUInt64(json: Json): Either[RippleCodecError, RawValue] = {
    parseUInt64(json).flatMap(encodeULong(_, "UInt64"))
  }



  /** Not sure what the Ripple spec, but we should assume all numbers are Base 10 in ""
    * for UInt64. This is as opposed to BigInt for some other numbers in quotes (e.g. Drops) */
  def parseUInt64(json: Json): Either[RippleCodecError, ULong] = {
    BinCodecExeption.wrap(s"Decoding JSON as UInt64"){

      val raw: Either[OErrorRipple, String] =
        Either.fromOption(json.asString, RippleCodecError(s"$json wasnt a string"))

      val longer: Either[RippleCodecError, ULong] = raw.flatMap{ v ⇒
        Try{
          BigInt(v, 10)
        }.recover{
          case e: java.lang.NumberFormatException ⇒ BigInt(v, 16)
        }
        .toEither
        .leftMap((t: Throwable) ⇒ BinCodecExeption(t))
        .flatMap(ByteUtils.bigInt2ulong)

      }
      longer
    }
  }

}

object UIntCodecs extends UIntCodecs
