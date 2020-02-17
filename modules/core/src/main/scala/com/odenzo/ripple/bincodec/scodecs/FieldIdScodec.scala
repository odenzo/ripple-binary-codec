package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._
import cats.implicits._
import com.odenzo.ripple.bincodec.models.FieldId
import scodec._
import scodec.codecs._
import scodec.bits._

/** This is for using Scodec Properly, with an initial focus on Decoding */
trait FieldIdScodec {

  val typeAndField: Codec[FieldId] = (constant(hex"00") ~> uint8 ~ uint8)
    .xmap(FieldId.fromTypeAndField, id => (id.typeCode, id.fieldCode))

  val smallFieldAndType: Codec[FieldId] = (constant(bin"0000") ~> uint4 ~ uint8)
    .xmap(FieldId.fromFieldAndType, id => (id.fieldCode, id.typeCode))

  val smallTypeAndField: Codec[FieldId] = ((uint4 <~ constant(bin"0000")) ~ uint8)
    .xmap(FieldId.fromTypeAndField, id => (id.typeCode, id.fieldCode))

  val smallTypeAndSmallField: Codec[FieldId] = (uint4 ~ uint4)
    .xmap(FieldId.fromTypeAndField, id => (id.typeCode, id.fieldCode))

  // For encoding we need to reverse the choice list.
  // Maybe a peek for decoding is better and  have a different encoder and decoder
  // If typecode is > 10000. e.g. 10001 then its a Mnemonic thing, and data type is UINT16 but Json is a String
  // So, we will look information if round-trip it here, FieldCode is 1, and other FieldCode 1 with UINT16 datatype
  // WTF, actually FC 1 TC 1 is LedgerEntryType and there is a ledgerentry data type
  private val orderedChoice = List(typeAndField, smallTypeAndField, smallFieldAndType, smallTypeAndSmallField)

  val xrpfieldid: Codec[FieldId] = { // Can Peek
    Codec[FieldId](
      choice((orderedChoice.reverse): _*),
      choice(orderedChoice: _*)
    )
  }
}

object FieldIdScodec extends FieldIdScodec
