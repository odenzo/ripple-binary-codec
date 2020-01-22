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

  val typeAndName = (constant(hex"00") ~ suint8 ~ suint8)
    .flattenLeftPairs
    .dropUnits
    .as[(UInt, UInt)]
    .xmap[(UInt, UInt)](a => a.swap, b => b.swap)
    .xmap[FieldId](v => FieldId.tupled(v), v => (v.fName, v.fType))

  val smallTypeAndName = (constant(bin"0000") ~ suint4 ~ suint8)
    .flattenLeftPairs
    .dropUnits
    .as[(UInt, UInt)]
    .xmap[(UInt, UInt)](a => a.swap, b => b.swap)
    .xmap[FieldId](v => FieldId.tupled(v), v => (v.fName, v.fType))

  val smallNameAndType = (suint4 ~ constant(bin"0000") ~ suint8)
    .flattenLeftPairs
    .dropUnits
    .as[(UInt, UInt)]
    .xmap[FieldId](v => FieldId.tupled(v), v => (v.fName, v.fType))

  val smallTypeAndSmallName = (suint4 ~ suint4)
    .xmap[(UInt, UInt)](a => a.swap, b => b.swap)
    .xmap[FieldId](v => FieldId.tupled(v), v => (v.fName, v.fType))

  // Woops, for encoding we need to reverse the choice list. Maybe a peek for decoding is better and lets have a different encoder and
  // decoder
  val fieldid: Codec[FieldId] = {
    val orderedChoice = List(typeAndName, smallTypeAndName, smallNameAndType, smallTypeAndSmallName)
    Codec[FieldId](
      choice((orderedChoice.reverse): _*),
      choice(orderedChoice: _*)
    )
  }

  //  def encodeFieldID(fName: Int, fType: Int): BitVector = {
//    scribe.debug(s"Encoding Field $fName and Data Type: $fType")
//    val fieldCode = UByte(fName)
//    val typeCode  = UByte(fType)
//
//    val fcBig = fieldCode >= UByte(16)
//    val tcBig = typeCode >= UByte(16)
//
//    val packed: List[UByte] = (fcBig, tcBig) match {
//      case (false, false) => ((typeCode << 4) | fieldCode) :: Nil
//      case (true, false)  => (typeCode << 4) :: fieldCode :: Nil
//      case (false, true)  => fieldCode :: typeCode :: Nil
//      case (true, true)   => UByte(0) :: typeCode :: fieldCode :: Nil
//    }
//    import com.odenzo.scodec.spire._
//    packed.map(subyte.encode(_).require).reduce(_ ++ _)
//  }

}
