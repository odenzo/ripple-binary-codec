package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions, FieldMetaData}
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.{DecodedField, RawValue}

/**
  * Better dealing with definitions data ?
  */
trait CodecUtils {

  val dd: DefinitionData = Definitions.fieldData

  /** TODO: Potentially overflow, check thus esp UInt64 */
  def encodeULong(value: ULong, dataType: String): Either[RippleCodecError, RawValue] = {
     dataType match {
      case "UInt8"  ⇒ encodeULong(value,1)
      case "UInt16" ⇒ encodeULong(value, 2)
      case "UInt32" ⇒ encodeULong(value, 4)
      case "UInt64" ⇒ encodeULong(value, 8)
      case other    ⇒ RippleCodecError(s"$other was not a valid unsigned int type").asLeft
    }

  }


  def encodeULong(value: ULong, numBytes: Int): Either[RippleCodecError, RawValue] = {
    val bytes: Either[RippleCodecError, List[UByte]] = (0 until numBytes)
      .map{ i: Int ⇒
        val calcUnsigned: ULong = value >>> (i * 8)
        UByte(calcUnsigned.toByte)
      }
      .toList
      .reverse
      .asRight[RippleCodecError]

    bytes.foreach(bl ⇒ assert(bl.length === numBytes))
    bytes.map(RawValue)
  }

  /** Decodes field bytes to hex with no padding */
  def decodeToUBytes(numBytes: Int,
                     v: List[UByte],
                     info: FieldMetaData): Either[OErrorRipple, (DecodedField, List[UByte])] = {
    if (numBytes > v.length) RippleCodecError(s"$numBytes exceeded length ${v.length} decoding").asLeft
    else {
      val (taken: List[UByte], remaining) = v.splitAt(numBytes)
      (DecodedField(info, taken), remaining).asRight
    }
  }
}
