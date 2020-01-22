package com.odenzo.ripple.bincodec

import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import spire.math.UInt

class FieldIdScodecTest extends OTestSpec {

  import com.odenzo.ripple.bincodec.scodecs.FieldIdScodec._

  test("SmallSmall Encoding") {
    val codec   = smallTypeAndSmallName
    val fName   = UInt(15)
    val fType   = UInt(1)
    val fieldId = FieldId(fName, fType)
    codec.encode(FieldId(fName, fType)).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      smallTypeAndSmallName.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  test("SmallType Name Encoding") {
    val codec   = smallTypeAndName
    val fName   = 255
    val fType   = 1
    val fieldId = FieldId(UInt(fName), UInt(fType))
    codec.encode(fieldId).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      codec.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  test("Type Small Name Encoding") {
    val codec   = smallNameAndType
    val fName   = 15
    val fType   = 255
    val fieldId = FieldId(UInt(fName), UInt(fType))
    codec.encode(fieldId).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      codec.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  test("Type  Name Encoding") {
    val codec   = typeAndName
    val fName   = 255
    val fType   = 255
    val fieldId = FieldId(UInt(fName), UInt(fType))
    codec.encode(fieldId).map { bv =>
      scribe.info(s"BV = ${bv.toBin}")
      codec.decode(bv).map { decRes =>
        scribe.info(s"Decoded Res: $decRes")
      }
    }
  }

  def roundTripFieldId(fName: UInt, fType: UInt) = {
    val tup     = (fName.toInt, fType.toInt)
    val fieldId = FieldId((fName), (fType))

    scribe.debug(s"Encoding Tuple $tup")
    val bitv = fieldid.encode(fieldId).require
    scribe.info(s"For $tup \t Size: ${bitv.bytes.size} Bits: ${bitv.toBin}")
    val ans: DecodeResult[FieldId] = fieldid.decode(bitv).require
    scribe.info(s"Ans: $ans")
  }

  test("Round Small Small") {
    roundTripFieldId(UInt(10), UInt(10))
  }
}
