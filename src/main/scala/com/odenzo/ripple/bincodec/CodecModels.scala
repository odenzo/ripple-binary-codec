package com.odenzo.ripple.bincodec

import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{FieldData, FieldInfo, RippleDataType}
import com.odenzo.ripple.bincodec.utils.ByteUtils

sealed trait CodecValue

sealed trait Decoded   extends CodecValue


sealed trait Encoded extends CodecValue  {
   val encoded: List[RawValue]

  /**
    *
    * @return Linearized bytes from this and all nested objects rolled up
    */
  lazy val rawBytes: List[UByte] = encoded.flatMap(_.ubytes)

  def toHex: String = ByteUtils.ubytes2hex(rawBytes)

  def toBytes: Array[Byte] = rawBytes.map(_.toByte).toArray
}


case class DecodedField(fi: FieldInfo, value: List[UByte]) extends Decoded

case class DecodedNestedField(fi: FieldInfo, nested: List[Decoded]) extends Decoded

case class DecodedUBytes(value: List[UByte]) extends Decoded

case class RawValue(ubytes: Seq[UByte]) extends Encoded with Decoded {
  lazy val encoded: List[RawValue] = List(this)
}


case class EncodedField(fieldValue: Encoded, data: FieldData) extends Encoded {
  lazy val encoded: List[RawValue] = data.fi.fieldID +: fieldValue.encoded
}

case class EncodedDataType(value: RawValue, rtype: RippleDataType) extends Encoded {
  lazy val encoded: List[RawValue] = List(value)

}

/**
  * @param enclosed Should ne NonEmptyList
  */
case class EncodedNestedVals(enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded)


  def fieldsInOrder: List[String] = {
    enclosed.flatMap{
      case fe: EncodedField ⇒ Some(fe.data.fieldName)
      case other            ⇒ None
    }
  }
}

/**
  *
  * @param vl     The calculated length of ubytes with Ripple special encoding
  * @param ubytes The raw data, prior to being VL encoded
  */
case class EncodedVL(vl: RawValue, ubytes: RawValue) extends Encoded {
  lazy val encoded: List[RawValue] = vl.encoded ::: ubytes.encoded
}

/** For null type objects, e.g. an empty array of memos */
case object EmptyValue extends Encoded {
  val encoded: List[RawValue] = List.empty[RawValue]
}


