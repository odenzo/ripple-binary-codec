package com.odenzo.ripple.bincodec.codecs

import scala.util.Try

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.{ULong, UByte}

import com.odenzo.ripple.bincodec.{BCException, RawValue, BinCodecLibError}
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}

trait UIntCodecs extends CodecUtils {

  /** Adapted from the Javascript
    * Redo with one UInt Decoder and a function for each type.
    * */
  def encodeUIntN(v: Json, dataType: String): Either[BinCodecLibError, RawValue] = {
    BinCodecLibError.handlingM(s"Binary Encoding JSON # $v") {

      val number: Option[ULong] = for {
        numeric <- v.asNumber
        ulong   <- numeric.toLong.map(ULong(_))
      } yield ulong

      val unsigned = Either.fromOption(number, BinCodecLibError(s"JSON ${v.spaces2} was not a Unisgned Long Number"))

      val ans: Either[BinCodecLibError, RawValue] = unsigned.flatMap(v => encodeULong(v, dataType))
      ans
    }
  }
  def encodeUInt8(json: Json): Either[BinCodecLibError, RawValue]  = encodeUIntN(json, "UInt8")
  def encodeUInt16(json: Json): Either[BinCodecLibError, RawValue] = encodeUIntN(json, "UInt16")
  def encodeUInt32(json: Json): Either[BinCodecLibError, RawValue] = encodeUIntN(json, "UInt32")
  def encodeUInt64(json: Json): Either[BinCodecLibError, RawValue] = parseUInt64(json).flatMap(encodeULong(_, "UInt64"))

  /** Not sure what the Ripple spec, but we should assume all numbers are Base 10 in ""
    * for UInt64. This is as opposed to BigInt for some other numbers in quotes (e.g. Drops) */
  def parseUInt64(json: Json): Either[BinCodecLibError, ULong] = {

    JsonUtils.json2string(json).flatMap { uval =>
      BinCodecLibError.handlingM(s"Decoding JSON  $uval as UInt64") {
        Try {
          BigInt(uval, 10)
        }.recover {
            case e: java.lang.NumberFormatException => BigInt(uval, 16)
          }
          .toEither
          .leftMap((t: Throwable) => BCException(err = t))
          .flatMap(ByteUtils.bigInt2ulong)

      }
    }
  }

  /** Turns a ULong into a sequence of ubytes based on the type */
  def encodeULong(value: ULong, dataType: String): Either[BinCodecLibError, RawValue] = {
    dataType match {
      case "UInt8"  => encodeULong(value, 1)
      case "UInt16" => encodeULong(value, 2)
      case "UInt32" => encodeULong(value, 4)
      case "UInt64" => encodeULong(value, 8) // Danger
      case other    => BinCodecLibError(s"$other was not a valid unsigned int type").asLeft
    }

  }

  /** Uses ULong to hold various sizes of UInt */
  def encodeULong(value: ULong, numBytes: Int): Either[BinCodecLibError, RawValue] = {
    val bytes: Either[BinCodecLibError, List[UByte]] = (0 until numBytes)
      .map { i: Int =>
        val calcUnsigned: ULong = value >>> (i * 8)
        UByte(calcUnsigned.toByte)
      }
      .toList
      .reverse
      .asRight[BinCodecLibError]

    bytes.foreach(bl => assert(bl.length === numBytes))
    bytes.map(RawValue.apply)
  }

}

object UIntCodecs extends UIntCodecs
