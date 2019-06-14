package com.odenzo.ripple.bincodec.reference

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.{UByte, UInt}

import com.odenzo.ripple.bincodec.serializing.BinarySerializer.RawEncodedValue
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}

/**
  *
  *
  * @param name  Name of the "type" of the field
  * @param value used as ordinal in javascript to order the fields in an object
  */
case class RippleType(name: String, value: Long)

object RippleType {}

/** Note that tipe is a String matching kv */
case class FieldType(nth: Long, isVLEncoded: Boolean, isSerialized: Boolean, isSigningField: Boolean, tipe: String)

case class FieldInfo(nth: Long,
                     isVLEncoded: Boolean,
                     isSerialized: Boolean,
                     isSigningField: Boolean,
                     fieldType: RippleType) {

  def fieldTypeName: String    = fieldType.name
  val fieldID: RawEncodedValue = RawEncodedValue(FieldInfo.encodeFieldID(nth.toInt, fieldType.value.toInt))
  val sortKey: UInt            = UInt(fieldType.value) << 16 | UInt(nth)

}

object FieldInfo extends StrictLogging {

  /*
   * Encodes the field id marker by field code and type code.
   * https://developers.ripple.com/serialization.html#field-ids
   */
  def encodeFieldID(fName: Int, fType: Int): List[UByte] = {
    // This encoding is complicated. The value in the type and the value for the fieldName.
    // If fieldName fits in one byte do that. Else expand to two bytes.
    // See: datadriventest.json for fixtures
    // fName maps to nth of type, or nth in definitions.json.
    // Name -1 for invalid until 259. So I guess 1,2,3 bytes?
    // 16, 16 = 0x01010
    //

    val fieldCode = UByte(fName)
    val typeCode  = UByte(fType)

    val fcBig = fieldCode >= UByte(16)
    val tcBig = typeCode >= UByte(16)

    // Refactored to match the found documentation
    // One two or three BYTES
    val packed: List[UByte] = (fcBig, tcBig) match {
      case (false, false) ⇒ ((typeCode << 4) | fieldCode) :: Nil
      case (true, false)  ⇒ (typeCode << 4) :: fieldCode :: Nil
      case (false, true)  ⇒ fieldCode :: typeCode :: Nil
      case (true, true)   ⇒ UByte(0) :: typeCode :: fieldCode :: Nil
    }
    packed
  }

  def decodeFieldId(ubytes: List[UByte]) = {
    // This can be one two or three bytes.
    val ZERO             = UByte.MinValue
    val TOP_FOUR_MASK    = UByte(0xF0)
    val BOTTOM_FOUR_MASK = UByte(0x0F)

    if (ubytes.head === ZERO) {
      // case true true from encoding
      val typecode  = ubytes(1)
      val fieldcode = ubytes(2)
      (typecode, fieldcode)

    } else if ((ubytes.head & TOP_FOUR_MASK) === ZERO) {
      val fielcode = ubytes.head
      val typecode = ubytes(1)
    } else if ((ubytes.head & BOTTOM_FOUR_MASK) === ZERO) {
      // Typecode in top 4 bits
      // Field Code in second byte
      val typecode  = ubytes.head >> 4
      val fieldcode = ubytes(1)

    } else {
      // One byte, top 4 is type, bottom 4 field code
      val fieldcode = ubytes.head & BOTTOM_FOUR_MASK
      val typecode  = (ubytes.head & TOP_FOUR_MASK) >> 4
    }
  }
}

object FieldType {

  import io.circe._

  implicit val decodeFieldType: Decoder[FieldType] =
    Decoder.forProduct5("nth", "isVLEncoded", "isSerialized", "isSigningField", "type")(FieldType.apply)

}

/**
  *   Caution, plenty of room for error with same type signature.
  * @param fieldsInfo
  * @param fieldsData
  * @param types
  * @param ledgerTypes
  * @param txnTypes
  * @param txnResultTypes
  */
case class DefinitionData(fieldsInfo: Map[String, FieldInfo],
                          fieldsData: Map[String, FieldType],
                          types: Map[String, Long],
                          ledgerTypes: Map[String, Long],
                          txnTypes: Map[String, Long],
                          txnResultTypes: Map[String, Long])
    extends StrictLogging {

  private def getMapEntry[T, V](map: Map[T, V], key: T): Either[OErrorRipple, V] = {
    Either.fromOption(map.get(key), RippleCodecError(s" $key not found in map"))
  }

  def getFieldInfo(name: String): Either[OErrorRipple, FieldInfo] = getMapEntry(fieldsInfo, name)

  def getTypeObj(name: String): Either[OErrorRipple, RippleType] = getType(name).map(RippleType(name, _)) // Optimize

  def getType(name: String): Either[OErrorRipple, Long] = getMapEntry(types, name)

  // We should map these to UInt16 bytes as they are always encoded that way
  def getTransactionType(txn: String): Either[OErrorRipple, Long] = getMapEntry(txnTypes, txn)

  // We should map these to UInt16 bytes as they are always encoded that way
  def getLedgerEntryType(lt: String): Either[OErrorRipple, Long] = getMapEntry(ledgerTypes, lt)

  def getTxnResultType(lt: String): Either[OErrorRipple, Long] = getMapEntry(txnResultTypes, lt)

  def rangeOfNth: (FieldType, FieldType) = {
    val min: FieldType = fieldsData.values.minBy(_.nth)
    val max            = fieldsData.values.maxBy(_.nth)
    logger.debug(s"Range of Nth: \n $min \n $max")
    (min, max)
  }

  /** Each field has a marker, for debugging I find the fieldinfo from that
    * Meh, easier to do this via bytes*/
  def findByFieldMarker(hex: String): Option[(String, FieldInfo)] = {
    val ms = fieldsInfo.toList.filter(_._2.fieldID.toHex == hex).toList
    ms.foreach(ms => logger.info(s" Field Info $hex: $ms"))
    ms.headOption
  }
}

object DefinitionData {
  val pathSetAnother  = RawEncodedValue(List(UByte(0xFF))) //  FF indicates another path follows
  val pathSetEnd      = RawEncodedValue(List(UByte(0x00))) // 00 indicates the end of the PathSet
  val objectEndMarker = RawEncodedValue(List(UByte(0xE1))) // 0xE1, this is STObject not blob
  val arrayEndMarker  = RawEncodedValue(List(UByte(0xF1)))

}
