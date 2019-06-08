package com.odenzo.ripple.bincodec.serializing

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe._
import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions, FieldInfo, RippleType}
import com.odenzo.ripple.bincodec.serializing.BinarySerializer.{
  EmptyValue,
  Encoded,
  FieldEncoded,
  NestedEncodedValues,
  RawEncodedValue,
  RippleTypeEncoded,
  VLEncodedValue
}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, Formatter}

object BinarySerializer extends StrictLogging {

  val defns: DefinitionData = Definitions.fieldData

  // These should be in Reference Data
  val objDel: List[UByte] = List(UByte(15))

  val arrDel: List[UByte] = List(UByte(14))
  // FIXME: key is redundant
  case class FieldData(key: String, v: Json, fi: FieldInfo)

  sealed trait Encoded {
    // Returns the Encoded value for a field or data type, including FieldType
    //def encoded[T <: Encoded]: List[T]

    /** This gets the list of encoded objects, which may contain other encoded objects
      * Some of the encoded objects may be storing parts of their encoding seperately
      * This allows normalization and additional ADT like VLEncoded(vl:Encoded, payload:Encoded)
      **/
    val encoded: List[RawEncodedValue]

    /**
      *
      * @return Linearized bytes from this and all nested objects rolled up
      */
    lazy val rawBytes: List[UByte] = encoded.flatMap(_.ubytes)
    def toHex: String              = ByteUtils.ubytes2hex(rawBytes)
    def toBytes: Array[Byte]       = rawBytes.map(_.toByte).toArray
  }

  case class FieldEncoded(fieldValue: Encoded, data: FieldData) extends Encoded {
    lazy val encoded: List[RawEncodedValue] = data.fi.fieldID +: fieldValue.encoded
  }

  case class RippleTypeEncoded(value: RawEncodedValue, rtype: RippleType) extends Encoded {
    lazy val encoded: List[RawEncodedValue] = List(value)

  }

  /**
    *  e.g. STObject or STArray, so can PathSet
    *  can produce a list/tree of Encoded objects, some with FieldData some  not.
    *  @param enclosed  Should ne NonEmptyList
    */
  case class NestedEncodedValues(enclosed: List[Encoded]) extends Encoded {
    lazy val encoded: List[RawEncodedValue] = enclosed.flatMap(_.encoded)

    def fieldsInOrder: List[String] = {
      enclosed.flatMap {
        case fe: FieldEncoded ⇒ Some(fe.data.key)
        case other            ⇒ None
      }
    }
  }

  /**
    *
    * @param vl  The calculated length of ubytes with Ripple special encoding
    * @param ubytes The raw data, prior to being VL encoded
    */
  case class VLEncodedValue(vl: RawEncodedValue, ubytes: RawEncodedValue) extends Encoded {
    lazy val encoded: List[RawEncodedValue] = vl.encoded ::: ubytes.encoded
  }

  /** Would like to (mostly redundant) add RippleType since each atom is some kind of RippleType
    * But some internal thigs like Var Len encoding List[UByte] do not have.
    *
    * */
  case class RawEncodedValue(ubytes: Seq[UByte]) extends Encoded {
    lazy val encoded: List[RawEncodedValue] = List(this)
  }

  /** For null type objects, e.g. an empty array of memos */
  case object EmptyValue extends Encoded {
    val encoded: List[RawEncodedValue] = List.empty[RawEncodedValue]
  }
}

object DebuggingShows {
  private def dump[T](v: T): String = "SHOWN: " + Formatter.prettyPrint(v)

  val oDump: Any ⇒ String = dump

  implicit val showEncoded: Show[Encoded] = Show.show {
    // I thought there was a way around this if sealed trait
    case x: NestedEncodedValues ⇒ x.show
    case x: FieldEncoded        ⇒ x.show
    case x: RawEncodedValue     ⇒ x.show
    case x: RippleTypeEncoded   ⇒ x.show
    case x: VLEncodedValue      ⇒ x.show
    case EmptyValue             ⇒ "{Empty Value}"
  }

  implicit val showRawEV: Show[RawEncodedValue] = Show.show(v ⇒ s" Hex:[${v.toHex}]")

  implicit val showNEV: Show[NestedEncodedValues] = Show.show { nev ⇒
    "Nested: \n" +
      nev.enclosed.map((v: Encoded) ⇒ v.show).mkString("\n\t", "\n\t", "\n\n")
  }

  implicit val showRTenc: Show[RippleTypeEncoded] = Show.show { tre ⇒
    "RTE: " + tre.value.show + s"type = ${tre.rtype}"
  }

  implicit val showField: Show[FieldEncoded] = Show.show { fe ⇒
    fe.data.key + "\t" + fe.data.fi.fieldTypeName + "\t: " + fe.encoded.show
  }

  implicit val showVLEnc: Show[VLEncodedValue] = Show.show { vlenc ⇒
    "VLENC: " + vlenc.vl.show + " " + vlenc.ubytes.show
  }
}
