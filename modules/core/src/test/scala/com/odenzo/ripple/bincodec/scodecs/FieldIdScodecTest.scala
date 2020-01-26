package com.odenzo.ripple.bincodec.scodecs

import scodec.DecodeResult
import spire.math.UInt

import com.odenzo.ripple.bincodec.OTestSpec

/** Inidivudual Codecs Work, need a Combinator */
class FieldIdScodecTest extends OTestSpec {

  import com.odenzo.ripple.bincodec.scodecs.FieldIdScodec._

  test("SmallSmall Encoding") {
    val codec = smallTypeAndSmallField
    val fName = UInt(15)
    val fType = UInt(1)

    codec.encode((fName, fType)).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      smallTypeAndSmallField.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  test("SmallType Name Encoding") {
    val codec   = smallTypeAndField
    val fName   = 255
    val fType   = 1
    val fieldId = (UInt(fName), UInt(fType))
    codec.encode(fieldId).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      codec.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  test("Type Small Name Encoding") {
    val codec   = smallFieldAndType
    val fName   = 15
    val fType   = 255
    val fieldId = (UInt(fName), UInt(fType))
    codec.encode(fieldId).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      codec.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  test("Type  Name Encoding") {
    val codec   = typeAndField
    val fName   = 255
    val fType   = 255
    val fieldId = (UInt(fName), UInt(fType))
    codec.encode(fieldId).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      codec.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  def roundTripFieldId(fName: UInt, fType: UInt): Unit = {
    val tup     = (fName.toInt, fType.toInt)
    val fieldId = ((fName), (fType))

    scribe.debug(s"Encoding Tuple $tup")
    val bitv = xrpfieldid.encode(fieldId).require
    scribe.info(s"For $tup \t Size: ${bitv.bytes.size} Bits: ${bitv.toBin}")
    val ans: DecodeResult[(FieldCode, TypeCode)] = xrpfieldid.decode(bitv).require
    scribe.info(s"Ans: $ans")
  }

  test("Round Small Small") {
    roundTripFieldId(UInt(10), UInt(10))
  }
}
