package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.{Show, _}
import io.circe.JsonObject
import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{DefinitionData, FieldData, RippleDataType, FieldMetaData}
import com.odenzo.ripple.bincodec.utils.ByteUtils

sealed trait CodecValue

trait Decoded extends CodecValue

trait Encoded extends CodecValue {

  /**
    *
    * @return Linearized bytes from this and all nested objects rolled up
    */
  lazy val rawBytes: List[UByte] = encoded.flatMap(_.ubytes)
  val encoded: List[RawValue]

  def toHex: String        = ByteUtils.ubytes2hex(rawBytes)
  def toBytes: Array[Byte] = rawBytes.map(_.toByte).toArray
}

case class RawValue(ubytes: List[UByte]) extends Encoded with Decoded {
  lazy val encoded: List[RawValue] = List(this)
}

case class DecodedField(fi: FieldMetaData, ubytes: List[UByte]) extends Decoded

case class DecodedNestedField(fi: FieldMetaData, nested: List[Decoded]) extends Decoded

case class EncodedField(fieldValue: Encoded, data: FieldData) extends Encoded {
  lazy val encoded: List[RawValue] = fieldValue match {
    case EmptyValue => Nil
    case _          => data.fi.fieldID +: fieldValue.encoded
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

/** List of Paths, with each Path have n PathSteps */
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
case class EncodedSTObject(enclosed: List[EncodedField], isTopLevel: Boolean) extends Encoded {
  lazy val encoded: List[RawValue] = isTopLevel match {
    case false => enclosed.flatMap(_.encoded) ::: List(endMarker)
    case true  => enclosed.flatMap(_.encoded)
  }

  val endMarker: RawValue = DefinitionData.objectEndMarker
}

case class EncodedSTArray(enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded) ++ EncodedSTArray.endMarker
}

/** Vector256, this is VLEncoded, and data type is always Hash256, but just use RawEncoded */
case class EncodedVector256(vl: RawValue, enclosed: List[Encoded]) extends Encoded {
  lazy val encoded: List[RawValue] = enclosed.flatMap(_.encoded) ++ endMarker
  val endMarker: List[RawValue]    = List(DefinitionData.arrayEndMarker)
}

/** For null type objects, e.g. an empty array of memos. Can be used to replace  payload (e.g. RawValue)
  *  or the EncodedField even. If EncodedField has fieldValue as EmptyValue is probably should be excluded
  *  from BinarySerialization no matter what.
  **/
case object EmptyValue extends Encoded with Decoded {
  val encoded: List[RawValue] = Nil
}

// --------- Companian objects with Shows -----------

object Encoded {
  implicit val showEncoded: Show[Encoded] = Show.show {
    // I thought there was a way around this if sealed trait
    case x: EncodedSTObject    => x.show
    case x: EncodedVector256   => x.show
    case x: EncodedField       => x.show
    case x: RawValue           => x.show
    case x: EncodedSTArray     => x.show
    case x: EncodedDataType    => x.show
    case x: EncodedVL          => x.show
    case x: EncodedPathSet     => x.show
    case x: EncodedNestedValue => x.show
    case EmptyValue            => "<Empty Value>"
  }

  implicit val encodedNestedValueMonoid: Monoid[EncodedNestedValue] = new Monoid[EncodedNestedValue] {
    def empty: EncodedNestedValue = EncodedNestedValue(List.empty[Encoded])
    def combine(x: EncodedNestedValue, y: EncodedNestedValue): EncodedNestedValue =
      EncodedNestedValue(x.enclosed ::: y.enclosed)
  }

}

object EncodedSTObject {
  implicit val showEncStObject: Show[EncodedSTObject] = Show.show { nev =>
    s"\n[STObject (Is Top Level: ${nev.isTopLevel}]: " +
      nev.enclosed.map((v: Encoded) => v.show).mkString("\n\t", "\n\t", "\n\n") +
      s"<--[EndSTObject] ${if (!nev.isTopLevel) DefinitionData.objectEndMarker.toHex}\n"

  }

}

object EncodedVector256 {

  implicit val showEncVector256: Show[EncodedVector256] = Show.show { nev =>
    "\n[Vector256]: \n" +
      nev.enclosed.map((v: Encoded) => v.show).mkString("\n\t", "\n\t", "\n\n") +
      "\n<--[Vector256]\n"

  }

}

object EncodedVL {
  implicit val showVLEnc: Show[EncodedVL] = Show.show { vlenc =>
    " VLENC: " + vlenc.vl.show + " " + vlenc.ubytes.show
  }
}
object DecodedField {
  implicit val showFieldDecodedUBytes: Show[DecodedField] = Show.show { v =>
    v.fi.fieldID.show + ":" + ByteUtils.ubytes2hex(v.ubytes)
  }

}

object DecodedNestedField {
  implicit val showFieldDecodedNested: Show[DecodedNestedField] = Show.show { v =>
    val nested = v.nested.map(_.show).mkString("Nested\n\t", "\n\t", "\n\n")
    v.fi.fieldID.show + ":" + nested
  }
}

object RawValue {
  implicit val showRaw: Show[RawValue] = Show.show(v => s" [${v.toHex}]")

}

object Decoded {

  implicit val showFieldDecoded: Show[Decoded] = Show.show {
    case x: RawValue           => x.show
    case x: DecodedField       => x.show
    case x: DecodedNestedField => x.show
    case EmptyValue            => "<Empty Value>"
  }

}

object EncodedNestedValue {
  implicit val showEncNested: Show[EncodedNestedValue] = Show.show { nev =>
    "\n[Nested]: " +
      nev.enclosed.map((v: Encoded) => v.show).mkString("\n\t", "\n\t", "\n\n") +
      s"<--[NestdEnd]\n"

  }
}

object EncodedField {
  implicit val showField: Show[EncodedField] = Show.show { fe =>
    " [Field] " + fe.data.fieldName.padTo(21, ' ') +
      fe.data.fi.fieldTypeName.padTo(10, ' ') + " ID:" + fe.data.fi.fieldID.show.padTo(8, ' ') +
      fe.fieldValue.show
  }
}

object EncodedDataType {
  implicit val showDataType: Show[EncodedDataType] = Show.show { tre =>
    "RTE: " + tre.value.show + s"type = ${tre.rtype}"
  }

}
object EncodedPathSet {

  implicit val showPathSet: Show[EncodedPathSet] = Show.show { v =>
    " Pathset: " + v.enclosed.map((p: Encoded) => p.show).mkString(" : ")
  }

}

object EncodedSTArray {
  val endMarker: List[RawValue] = List(DefinitionData.arrayEndMarker)
  implicit val showEncArray: Show[EncodedSTArray] = Show.show { nev =>
    "\n[Array]: \n" +
      nev.enclosed.map((v: Encoded) => v.show).mkString("\n\t", "\n\t", "\n\n") +
      s"\n<--[EndArray:${DefinitionData.arrayEndMarker.toHex}]\n"

  }

}
