package com.odenzo.ripple.bincodec.scodecs

import scodec.DecodeResult
import scodec.bits.BitVector
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

  def roundTripFieldId(fName: UInt, fType: UInt): BitVector = {

    val fieldId = ((fName), (fType))

    val bitv = xrpfieldid.encode(fieldId).require
    //scribe.info(s"For $fieldId \t Size: ${bitv.bytes.size} Bits: ${bitv.toBin}")
    val ans: DecodeResult[(FieldCode, TypeCode)] = xrpfieldid.decode(bitv).require
    ans.value shouldEqual fieldId
    bitv
  }

  test("Round Small Small") {
    for {
      i <- 1 to 15
      j <- 1 to 15
      bits = roundTripFieldId(UInt(i), UInt(j))
      _    = bits.size shouldEqual 8
    } yield (i, j)
  }

  test("Round Small Medium") {
    for {
      i <- 1 to 15
      j <- 16 to 255
      bits = roundTripFieldId(UInt(i), UInt(j))
      _    = bits.size shouldEqual 16
    } yield (i, j)
  }

  test("Round Medium Small") {
    for {
      i <- 16 to 255
      j <- 1 to 15
      bits = roundTripFieldId(UInt(i), UInt(j))
      _    = bits.size shouldEqual 16
    } yield (i, j)
  }

  test("Round Big Big") {
    for {
      i <- 16 to 255
      j <- 16 to 255
      bits = roundTripFieldId(UInt(i), UInt(j))
      _    = bits.size shouldEqual 24
    } yield (i, j)
  }
}
