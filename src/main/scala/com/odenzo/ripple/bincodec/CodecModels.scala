package com.odenzo.ripple.bincodec

import scribe.Logging
import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{FieldData, FieldInfo, RippleDataType}
import com.odenzo.ripple.bincodec.utils.ByteUtils

sealed trait CodecValue

sealed trait Decoded   extends CodecValue


sealed trait Encoded extends CodecValue  with Logging {
   val encoded: List[RawValue]

  /**
    *
    * @return Linearized bytes from this and all nested objects rolled up
    */
  lazy val rawBytes: List[UByte] = encoded.flatMap(_.ubytes)

  def toHex: String = ByteUtils.ubytes2hex(rawBytes)

  def toBytes: Array[Byte] = rawBytes.map(_.toByte).toArray
}


case class RawValue(ubytes: List[UByte]) extends Encoded with Decoded {
  lazy val encoded: List[RawValue] = List(this)
}

case class DecodedField(fi: FieldInfo, ubytes: List[UByte]) extends Decoded

case class DecodedNestedField(fi: FieldInfo, nested: List[Decoded]) extends Decoded




case class EncodedField(fieldValue: Encoded, data: FieldData) extends Encoded {
  lazy val encoded: List[RawValue] = data.fi.fieldID +: fieldValue.encoded
}

case class EncodedDataType(value: RawValue, rtype: RippleDataType) extends Encoded {
  lazy val encoded: List[RawValue] = List(value)

}

/**
  *
  * @param vl     The calculated length of ubytes with Ripple special encoding
  * @param ubytes The raw data, prior to being VL encoded
  */
case class EncodedVL(vl: RawValue, ubytes: RawValue) extends Encoded {
  lazy val encoded: List[RawValue] = vl.encoded ::: ubytes.encoded
}


/**
  * @param enclosed Can be nested fields or other Encoded types.
  *                 Should ne NonEmptyList
  */
case class EncodedNestedVals(enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded)
}

/** For null type objects, e.g. an empty array of memos */
case object EmptyValue extends Encoded with Decoded {
  val encoded: List[RawValue] = List.empty[RawValue]
}


