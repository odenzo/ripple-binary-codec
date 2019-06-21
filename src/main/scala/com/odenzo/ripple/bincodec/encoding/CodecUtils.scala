package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions, FieldInfo}
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.{DecodedField, RawValue}

/**
  * Better dealing with definitions data ?
  */
trait CodecUtils   {

  val dd: DefinitionData = Definitions.fieldData


  /** TODO: Potentially overflow, check thus esp UInt64 */
  def encodeULong(value: ULong, dataType: String): Either[Nothing, RawValue] = {
    val fieldLength = dataType match {
      case "UInt8"  ⇒ 1
      case "UInt16" ⇒ 2
      case "UInt32" ⇒ 4
      case "UInt64" ⇒ 8
      case other    ⇒ throw new IllegalArgumentException(s"$other was not a valid unsigned int type")
    }

    val bytes: Either[Nothing, List[UByte]] = (0 until fieldLength)
      .map{ i: Int ⇒
        val calcUnsigned: ULong = value >>> (i * 8)
        UByte(calcUnsigned.toByte)
      }
      .toList
      .reverse
      .asRight
    bytes.foreach(bl ⇒ assert(bl.length == fieldLength))
    bytes.map(RawValue)
  }


  /** Decodes field bytes to hex with no padding */
  def decodeToUBytes(numBytes: Int,
                     v: List[UByte],
                     info: FieldInfo): Either[OErrorRipple, (DecodedField, List[UByte])] = {
    if (numBytes > v.length) RippleCodecError(s"$numBytes exceeded length ${v.length} decoding").asLeft
    else {
      val (taken: List[UByte], remaining) = v.splitAt(numBytes)
      (DecodedField(info, taken), remaining).asRight
    }
  }
}
