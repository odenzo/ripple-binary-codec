package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions, FieldMetaData}
import com.odenzo.ripple.bincodec.{DecodedField, BCLibErr, RawValue, BinCodecLibError}

/**
  * Better dealing with definitions data ?
  * These are used often enough might as well optimize
  */
trait CodecUtils {

  // This is a hack for convenience, for other things that  extend this trait
  val dd: DefinitionData = Definitions.fieldData

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

  /** Decodes field bytes to hex with no padding */
  def decodeToUBytes(
      numBytes: Int,
      v: List[UByte],
      info: FieldMetaData
  ): Either[BCLibErr, (DecodedField, List[UByte])] = {
    if (numBytes > v.length) BinCodecLibError(s"$numBytes exceeded length ${v.length} decoding").asLeft
    else {
      val (taken: List[UByte], remaining) = v.splitAt(numBytes)
      (DecodedField(info, taken), remaining).asRight
    }
  }
}
