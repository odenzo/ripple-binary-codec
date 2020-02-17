package com.odenzo.ripple.bincodec.models

import com.odenzo.ripple.bincodec.scodecs.FieldIdScodec
import scodec.bits.BitVector

case class FieldId(typeCode: Int, fieldCode: Int) {
  val adjustedTypeCode: Int   = if (typeCode > 1000) 16 else typeCode
  val orderKey: (Int, Int)    = (this.typeCode, this.fieldCode)
  lazy val encoded: BitVector = FieldId.encode(adjustedTypeCode, fieldCode)
}

object FieldId {

  def fromTypeAndField(tf: (Int, Int)): FieldId = (FieldId.apply _).tupled(tf)
  def fromFieldAndType(ft: (Int, Int)): FieldId = fromTypeAndField(ft.swap)

  def encode(typeCode: Int, fieldCode: Int): BitVector = {
    FieldIdScodec.xrpfieldid.encode(FieldId(typeCode, fieldCode)).require
  }
}
