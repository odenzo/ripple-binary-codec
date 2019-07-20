package com.odenzo.ripple.bincodec.syntax

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.reference.DefinitionData
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec._

object debugging {

  import ByteUtils._

  implicit val showEncoded: Show[Encoded] = Show.show {
    // I thought there was a way around this if sealed trait
    case x: EncodedSTObject    ⇒ x.show
    case x: EncodedVector256   ⇒ x.show
    case x: EncodedField       ⇒ x.show
    case x: RawValue           ⇒ x.show
    case x: EncodedSTArray     ⇒ x.show
    case x: EncodedDataType    ⇒ x.show
    case x: EncodedVL          ⇒ x.show
    case x: EncodedPathSet     ⇒ x.show
    case x: EncodedNestedValue ⇒ x.show
    case EmptyValue            ⇒ "<Empty Value>"
  }

  implicit val showFieldDecodedUBytes: Show[DecodedField] = Show.show { v ⇒
    v.fi.fieldID.show + ":" + ubytes2hex(v.ubytes)
  }

  implicit val showFieldDecodedNested: Show[DecodedNestedField] = Show.show { v ⇒
    val nested = v.nested.map(_.show).mkString("Nested\n\t", "\n\t", "\n\n")
    v.fi.fieldID.show + ":" + nested
  }

  implicit val showFieldDecoded: Show[Decoded] = Show.show {
    case x: RawValue           ⇒ x.show
    case x: DecodedField       ⇒ x.show
    case x: DecodedNestedField ⇒ x.show
    case EmptyValue            => "<Empty Value>"
  }

  implicit val showRaw: Show[RawValue] = Show.show(v ⇒ s" RawHex:[${v.toHex}]")

  implicit val showEncStObject: Show[EncodedSTObject] = Show.show { nev ⇒
    s"\n[STObject (Nested: ${nev.isNested}]: " +
      nev.enclosed.map((v: Encoded) ⇒ v.show).mkString("\n\t", "\n\t", "\n\n") +
      s"<--[EndSTObject] ${if (nev.isNested) DefinitionData.objectEndMarker.toHex}\n"

  }

  implicit val showEncNested: Show[EncodedNestedValue] = Show.show { nev ⇒
    "\n[Nested]: " +
      nev.enclosed.map((v: Encoded) ⇒ v.show).mkString("\n\t", "\n\t", "\n\n") +
      s"<--[NestdEnd]\n"

  }

  implicit val showEncVector256: Show[EncodedVector256] = Show.show { nev ⇒
    "\n[Vector256]: \n" +
      nev.enclosed.map((v: Encoded) ⇒ v.show).mkString("\n\t", "\n\t", "\n\n") +
      "\n<--[Vector256]\n"

  }

  implicit val showEncArray: Show[EncodedSTArray] = Show.show { nev ⇒
    "\n[Array]: \n" +
      nev.enclosed.map((v: Encoded) ⇒ v.show).mkString("\n\t", "\n\t", "\n\n") +
      s"\n<--[EndArray:${DefinitionData.arrayEndMarker.toHex}]\n"

  }

  implicit val showPathSet: Show[EncodedPathSet] = Show.show { v ⇒
    " Pathset: " + v.enclosed.map((p: Encoded) ⇒ p.show).mkString(" : ")
  }

  implicit val showDataType: Show[EncodedDataType] = Show.show { tre ⇒
    "RTE: " + tre.value.show + s"type = ${tre.rtype}"
  }

  implicit val showField: Show[EncodedField] = Show.show { fe ⇒
    " [Field] " + fe.data.fieldName + "\t Type: " + fe.data.fi.fieldTypeName + "\t: ID:" + fe.data.fi.fieldID.show +
      fe.fieldValue.show
  }

  implicit val showVLEnc: Show[EncodedVL] = Show.show { vlenc ⇒
    " VLENC: " + vlenc.vl.show + " " + vlenc.ubytes.show
  }

}
