package com.odenzo.ripple.bincodec

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.utils.ByteUtils

object syntax {

  object compact {

    import ByteUtils._

    // Compact we can use toHex on all but NestedVal?
    implicit val showEncoded: Show[Encoded] = Show.show {
      case x: EncodedNestedVals ⇒ x.show
      case x: EncodedField      ⇒ x.show
      case x: EncodedDataType   ⇒ x.toHex
      case x: EncodedVL         ⇒ x.toHex
      case x: RawValue          ⇒ x.toHex
      case EmptyValue           ⇒ "{Empty Value}"
    }

    implicit val showRaw: Show[RawValue] = Show.show[RawValue] { v ⇒
      ubytes2hex(v.ubytes)
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
      case EmptyValue            ⇒ "<Empty Value>"
    }

    implicit val showEncNested: Show[EncodedNestedVals] = Show.show { nev ⇒
      "Nested: \n" +
        nev.enclosed.map((v: Encoded) ⇒ v.show).mkString("\n\t", "\n\t", "\n\n")
    }

    implicit val showDataType: Show[EncodedDataType] = Show.show { tre ⇒
      "RTE: " + tre.value.show + s"type = ${tre.rtype}"
    }

    implicit val showField: Show[EncodedField] = Show.show { fe ⇒
      fe.data.fieldName + "\t" + fe.data.fi.fieldTypeName + "\t: " + fe.encoded.show
    }

    implicit val showVLEnc: Show[EncodedVL] = Show.show { vlenc ⇒
      "VLENC: " + vlenc.vl.show + " " + vlenc.ubytes.show
    }

  }

  object debugging {

    import ByteUtils._

    implicit val showEncoded: Show[Encoded] = Show.show {
      // I thought there was a way around this if sealed trait
      case x: EncodedNestedVals ⇒ x.show
      case x: EncodedField      ⇒ x.show
      case x: RawValue          ⇒ x.show
      case x: EncodedDataType   ⇒ x.show
      case x: EncodedVL         ⇒ x.show
      case EmptyValue           ⇒ "<Empty Value>"
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

    implicit val showRaw: Show[RawValue] = Show.show(v ⇒ s" Hex:[${v.toHex}]")

    implicit val showEncNested: Show[EncodedNestedVals] = Show.show { nev ⇒
      "Nested: \n" +
        nev.enclosed.map((v: Encoded) ⇒ v.show).mkString("\n\t", "\n\t", "\n\n")
    }

    implicit val showDataType: Show[EncodedDataType] = Show.show { tre ⇒
      "RTE: " + tre.value.show + s"type = ${tre.rtype}"
    }

    implicit val showField: Show[EncodedField] = Show.show { fe ⇒
      fe.data.fieldName + "\t" + fe.data.fi.fieldTypeName + "\t: " + fe.encoded.show
    }

    implicit val showVLEnc: Show[EncodedVL] = Show.show { vlenc ⇒
      "VLENC: " + vlenc.vl.show + " " + vlenc.ubytes.show
    }

  }

}