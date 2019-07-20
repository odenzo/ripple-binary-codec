package com.odenzo.ripple.bincodec

import scribe.Logging
import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{DefinitionData, FieldData, FieldMetaData, RippleDataType}
import com.odenzo.ripple.bincodec.utils.ByteUtils

sealed trait CodecValue

sealed trait Decoded extends CodecValue

sealed trait Encoded extends CodecValue with Logging {

  /**
    *
    * @return Linearized bytes from this and all nested objects rolled up
    */
  lazy val rawBytes: List[UByte] = encoded.flatMap(_.ubytes)
  val encoded: List[RawValue]

  def toHex: String = ByteUtils.ubytes2hex(rawBytes)

  def toBytes: Array[Byte] = rawBytes.map(_.toByte).toArray
}

case class RawValue(ubytes: List[UByte]) extends Encoded with Decoded {
  lazy val encoded: List[RawValue] = List(this)
}

case class DecodedField(fi: FieldMetaData, ubytes: List[UByte]) extends Decoded

case class DecodedNestedField(fi: FieldMetaData, nested: List[Decoded]) extends Decoded

case class EncodedField(fieldValue: Encoded, data: FieldData) extends Encoded {
  lazy val encoded: List[RawValue] = fieldValue match {
    case EmptyValue ⇒ EmptyValue.encoded
    case _          ⇒ data.fi.fieldID +: fieldValue.encoded
  }
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

case class EncodedPathSet(enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded)
}

case class EncodedNestedValue(enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded)
}

/**
  * @param enclosed Can be nested fields or other Encoded types.
  *                 Should ne NonEmptyList
  */
case class EncodedSTObject(enclosed: List[EncodedField], isNested: Boolean) extends Encoded {
  lazy val encoded: List[RawValue] = isNested match {
    case true  ⇒ enclosed.flatMap(_.encoded) ::: List(endMarker)
    case false ⇒ enclosed.flatMap(_.encoded)
  }

  val endMarker: RawValue = DefinitionData.objectEndMarker
}

case class EncodedSTArray(enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded) ++ endMarker
  val endMarker                    = List(DefinitionData.arrayEndMarker)
}

/** Vector256, this is VLEncoded, and data type is always Hash256, but just use RawEncoded */
case class EncodedVector256(vl: RawValue, enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded) ++ endMarker
  val endMarker                    = List(DefinitionData.arrayEndMarker)
}

/** For null type objects, e.g. an empty array of memos. Can be used to replace  payload (e.g. RawValue)
  *  or the EncodedField even. If EncodedField has fieldValue as EmptyValue is probably should be excluded
  *  from BinarySerialization no matter what.
  **/
case object EmptyValue extends Encoded with Decoded {
  val encoded: List[RawValue] = List.empty[RawValue]
}
