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

  // fType and fCode in that order make more sense
  case class FieldId(fName: UInt, fType: UInt)

  val nameFirst = (name: UInt, tipe: UInt) => FieldId(name, tipe)
  val typeFirst = (tipe: UInt, name: UInt) => FieldId(name, tipe)

  val genResp: Aux[(Int, Int), Int :: Int :: HNil] = Generic[(Int, Int)]

  /** Decode Logic:
    *  + ubyte(0) ~ ubyte ~ ubyte => typeCode,fieldCode
    *  + x: bits(4) ~ y: bits(4) :
    *      if x and y are non-zero => typeCode = x, fieldCode = y
    *      if x and y are zero => ubyte ~ ubyte => typeCode, fieldCode
    *
    *      if x noneZero => typeCode =x  fieldCode = ubyte
    *      else fieldCode = x :: y , typeCode = ubyte
    * */
  implicit val g = Generic[FieldId] //.xmap[(UInt, UInt)](v => v, v => v)

  val typeAndName = (constant(hex"00") ~> suint8 ~ suint8)
    .xmap[FieldId](typeFirst.tupled, v => (v.fType, v.fName))

  val smallTypeAndName = (constant(bin"0000") ~> suint4 ~ suint8)
    .xmap[FieldId](typeFirst.tupled, v => (v.fType, v.fName))

  val smallNameAndType = ((suint4 <~ constant(bin"0000")) ~ suint8)
    .xmap[FieldId](nameFirst.tupled, v => (v.fName, v.fType))

  val smallTypeAndSmallName = (suint4 ~ suint4)
    .xmap[FieldId](typeFirst.tupled, v => (v.fType, v.fName))

  // Woops, for encoding we need to reverse the choice list. Maybe a peek for decoding is better and lets have a different encoder and
  // decoder
  val fieldid: Codec[FieldId] = {
    val orderedChoice = List(typeAndName, smallTypeAndName, smallNameAndType, smallTypeAndSmallName)
    Codec[FieldId](
      choice((orderedChoice.reverse): _*),
      choice(orderedChoice: _*)
    )
  }

//  val fieldID: BitVector = FieldMetaData.encodeFieldID(nth.toInt, datatype.value.toInt)
//  val sortKey: UInt      = UInt(datatype.value) << 16 | UInt(nth)

  //  Try this approach: peak(1 byte) that can give us length and also code.   zero/zero = 3 0/not0 - 2, notz,0 -2 not/not = 1
  //val foo = peekVariableSizeXXX()

}
