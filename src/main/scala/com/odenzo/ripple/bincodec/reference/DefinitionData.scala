package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.{UByte, UInt}

import com.odenzo.ripple.bincodec.RawValue
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}

/**
  *
  * Holds information about the datatype
  *
  * @param name  Name of the "type" of the field
  * @param value used as ordinal in javascript to order the fields in an object
  */
case class RippleDataType(name: String, value: Long)

/** Note that tipe is a String matching kv */
case class FieldType(nth: Long, isVLEncoded: Boolean, isSerialized: Boolean, isSigningField: Boolean, tipe: String)

/** FeildType merged with RippleDataType */
case class FieldInfo(name: String,
                     nth: Long,
                     isVLEncoded: Boolean,
                     isSerialized: Boolean,
                     isSigningField: Boolean,
                     datatype: RippleDataType) {

  def fieldTypeName: String = datatype.name
  val fieldID: RawValue     = RawValue(FieldInfo.encodeFieldID(nth.toInt, datatype.value.toInt))
  val sortKey: UInt         = UInt(datatype.value) << 16 | UInt(nth)

}

/**
  *
  * @param fieldName The name of the field (?)
  * @param v         Value of the field in Json format
  * @param fi        Metadata about the field and its type
  */
case class FieldData(fieldName: String, v: Json, fi: FieldInfo)

object FieldInfo  {

  /*
   * Encodes the field id marker by field code and type code.
   * https://developers.ripple.com/serialization.html#field-ids
   */
  def encodeFieldID(fName: Int, fType: Int): List[UByte] = {
    scribe.debug(s"Encoding Field $fName and Data Type: $fType")
    val fieldCode = UByte(fName)
    val typeCode  = UByte(fType)

    val fcBig = fieldCode >= UByte(16)
    val tcBig = typeCode >= UByte(16)
    
    val packed: List[UByte] = (fcBig, tcBig) match {
      case (false, false) ⇒ ((typeCode << 4) | fieldCode) :: Nil
      case (true, false)  ⇒ (typeCode << 4) :: fieldCode :: Nil
      case (false, true)  ⇒ fieldCode :: typeCode :: Nil
      case (true, true)   ⇒ UByte(0) :: typeCode :: fieldCode :: Nil
    }
    packed
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
     {

  private def getMapEntry[T, V](map: Map[T, V], key: T): Either[OErrorRipple, V] = {
    Either.fromOption(map.get(key), RippleCodecError(s" $key not found in map"))
  }

  /** Tries to find the fieldnInfo for the fieldName. This may not exist (e.g. hash)
    * because the field is not (isSerialized or isSigning)
    *
    * @param fieldName
    * @param fieldValue
    *
    * @return
    */
  def optFieldData(fieldName: String, fieldValue: Json): Option[FieldData] = {
    findFieldInfo(fieldName).map { fi ⇒
      FieldData(fieldName, fieldValue, fi)
    }
  }

  def getFieldsByNth(nth: Long): Iterable[FieldInfo] = {
    fieldsInfo.filter(_._2.nth == nth).values
  }

  def getFieldData(fieldName: String, fieldValue: Json): Either[OErrorRipple, FieldData] = {
    Either.fromOption(optFieldData(fieldName, fieldValue), RippleCodecError(s"$fieldName not found"))
  }

  def getFieldInfo(name: String): Either[OErrorRipple, FieldInfo] = getMapEntry(fieldsInfo, name)

  def findFieldInfo(fieldName: String): Option[FieldInfo] = fieldsInfo.get(fieldName)

  def getTypeObj(name: String): Either[OErrorRipple, RippleDataType] =
    getType(name).map(RippleDataType(name, _)) // Optimize

  def getType(name: String): Either[OErrorRipple, Long] = getMapEntry(types, name)

  // Optimize We should map these to UInt16 bytes as they are always encoded that way
  def getTransactionType(txn: String): Either[OErrorRipple, Long] = getMapEntry(txnTypes, txn)

  // Optimize We should map these to UInt16 bytes as they are always encoded that way
  def getLedgerEntryType(lt: String): Either[OErrorRipple, Long] = getMapEntry(ledgerTypes, lt)

  def getTxnResultType(lt: String): Either[OErrorRipple, Long] = getMapEntry(txnResultTypes, lt)

  /** Each field has a marker, for debugging I find the fieldinfo from that
    * Meh, easier to do this via bytes*/
  def findByFieldMarker(ub: Seq[UByte]): Option[(String, FieldInfo)] = {
    val ms = fieldsInfo.filter((v: (String, FieldInfo)) ⇒ v._2.fieldID.ubytes == ub)
    ms.foreach(ms => scribe.info(s" Field Info ${ByteUtils.ubytes2hex(ub)}: $ms"))
    ms.headOption
  }

}

object DefinitionData {
  val pathSetAnother  = RawValue(List(UByte(0xFF))) //  FF indicates another path follows
  val pathSetEnd      = RawValue(List(UByte(0x00))) // 00 indicates the end of the PathSet
  val objectEndMarker = RawValue(List(UByte(0xE1))) // 0xE1, this is STObject not blob
  val arrayEndMarker  = RawValue(List(UByte(0xF1)))

  implicit val showFieldInfo: Show[FieldInfo] = Show[FieldInfo] { fi ⇒
    s"${fi.fieldID.toHex} : ${fi.name} ${fi.datatype.name} : VL ${fi.isVLEncoded} " +
      s" Serialized/Signing ${fi.isSerialized}/${fi.isSigningField} "
  }
  implicit val show: Show[DefinitionData] = Show[DefinitionData] { dd ⇒
    val sortedFields: List[(String, FieldInfo)] =
      dd.fieldsInfo.toList.sortBy(e ⇒ (e._2.fieldID.toHex.length, e._2.fieldID.toHex))

    sortedFields.map((v: (String, FieldInfo)) ⇒ v._2.show).mkString("\n")

  }
}
