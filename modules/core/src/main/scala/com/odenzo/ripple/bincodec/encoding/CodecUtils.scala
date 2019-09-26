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

object CodecUtils extends CodecUtils
