package com.odenzo.ripple.bincodec.scodecs

import spire.math.UInt
import scodec._
import scodec.codecs._
import scodec.bits._
import shapeless._
import shapeless.Generic.Aux

import com.odenzo.scodec.spire._

/** This is for using Scodec Properly, with an initial focus on Decoding */
object FieldIdScodec {

  type TypeCode  = Int
  type FieldCode = Int

  val swapFromOrder = (typeCode: TypeCode, fieldCode: FieldCode) => (fieldCode, typeCode)

  // Techincally this should be reversed types I think, but now new type yet for type checking
  val swapToOrder = (typeCode: TypeCode, fieldCode: FieldCode) => (fieldCode, typeCode)

  val typeAndField: Codec[(FieldCode, TypeCode)] = (constant(hex"00") ~> uint8 ~ uint8)
    .xmap(swapFromOrder, swapFromOrder)

  val smallFieldAndType: Codec[(FieldCode, TypeCode)] = (constant(bin"0000") ~> uint4 ~ uint8)

  val smallTypeAndField: Codec[(FieldCode, TypeCode)] = ((uint4 <~ constant(bin"0000")) ~ uint8)
    .xmap(swapFromOrder, swapFromOrder)

  val smallTypeAndSmallField = (uint4 ~ uint4)
    .xmap(swapFromOrder, swapFromOrder)

  // For encoding we need to reverse the choice list.
  // Maybe a peek for decoding is better and  have a different encoder and decoder
  // If typecode is > 10000. e.g. 10001 then its a Mnemonic thing, and data type is UINT16 but Json is a String
  // So, we will look information if round-trip it here, FieldCode is 1, and other FieldCode 1 with UINT16 datatype
  // WTF, actually FC 1 TC 1 is LedgerEntryType and there is a ledgerentry data type
  val xrpfieldid: Codec[(FieldCode, TypeCode)] = {
    val orderedChoice = List(typeAndField, smallTypeAndField, smallFieldAndType, smallTypeAndSmallField)
    Codec[(FieldCode, TypeCode)](
      choice((orderedChoice.reverse): _*),
      choice(orderedChoice: _*)
    )
  }
}
