package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import spire.math.{UByte, UInt}

import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.{BCLibErr, RawValue, BinCodecLibError}

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

object FieldType {

  implicit val config: Configuration =
    Configuration.default.copy(transformMemberNames = (s: String) => if (s === "tipe") "type" else s)
  implicit val codec: Codec.AsObject[FieldType] = deriveConfiguredCodec[FieldType]
}

/** FeildType merged with RippleDataType, with no data */
case class FieldMetaData(
    name: String,
    nth: Long,
    isVLEncoded: Boolean,
    isSerialized: Boolean,
    isSigningField: Boolean,
    datatype: RippleDataType
) {

  val fieldID: RawValue = RawValue(FieldMetaData.encodeFieldID(nth.toInt, datatype.value.toInt))
  val sortKey: UInt     = UInt(datatype.value) << 16 | UInt(nth)

  def fieldTypeName: String = datatype.name

}

/**
  * Json Field Data coupled with FieldInfo
  *
  * @param json Value of the field in Json format Like a Json | T
  * @param fi   Metadata about the field and its type
  */
case class FieldData(json: Json, fi: FieldMetaData) {
  final def fieldName: String = fi.name
}

object FieldMetaData {

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
      case (false, false) => ((typeCode << 4) | fieldCode) :: Nil
      case (true, false)  => (typeCode << 4) :: fieldCode :: Nil
      case (false, true)  => fieldCode :: typeCode :: Nil
      case (true, true)   => UByte(0) :: typeCode :: fieldCode :: Nil
    }
    packed
  }

}

/**
  * Caution, plenty of room for error with same type signature.
  *
  * @param fieldsInfo

  * @param types
  * @param ledgerTypes
  * @param txnTypes
  * @param txnResultTypes
  */
case class DefinitionData(
    fieldsInfo: Map[String, FieldMetaData],
    types: Map[String, Long],
    ledgerTypes: Map[String, Long],
    txnTypes: Map[String, Long],
    txnResultTypes: Map[String, Long]
) {

  /** Tries to find the fieldnInfo for the fieldName. This may not exist (e.g. hash)
    * because the field is not (isSerialized or isSigning)
    *
    * @param fieldName
    * @param fieldValue
    *
    * @return
    */
  def optFieldData(fieldName: String, fieldValue: Json): Option[FieldData] = {
    findFieldInfo(fieldName).map(fi => FieldData(fieldValue, fi))
  }

  def getFieldsByNth(nth: Long): Iterable[FieldMetaData] = {
    fieldsInfo.filter { case (_: String, fi: FieldMetaData) => fi.nth === nth }.values
  }

  def getFieldData(fieldName: String, fieldValue: Json): Either[BCLibErr, FieldData] = {
    Either.fromOption(optFieldData(fieldName, fieldValue), BinCodecLibError(s"$fieldName not found"))
  }

  def getFieldInfo(name: String): Either[BCLibErr, FieldMetaData] = getMapEntry(fieldsInfo, name)

  def findFieldInfo(fieldName: String): Option[FieldMetaData] = fieldsInfo.get(fieldName)

  def getTypeObj(name: String): Either[BCLibErr, RippleDataType] =
    getType(name).map(RippleDataType(name, _)) // Optimize

  def getType(name: String): Either[BCLibErr, Long] = getMapEntry(types, name)

  // Optimize We should map these to UInt16 bytes as they are always encoded that way
  def getTransactionType(txn: String): Either[BCLibErr, Long] = getMapEntry(txnTypes, txn)

  // Optimize We should map these to UInt16 bytes as they are always encoded that way
  def getLedgerEntryType(lt: String): Either[BCLibErr, Long] = getMapEntry(ledgerTypes, lt)

  def getTxnResultType(lt: String): Either[BCLibErr, Long] = getMapEntry(txnResultTypes, lt)

  /** Each field has a marker, for debugging I find the fieldinfo from that
    * Meh, easier to do this via bytes */
  def findByFieldMarker(ub: List[UByte]): Either[BCLibErr, FieldMetaData] = {
    val ms: Map[String, FieldMetaData] = fieldsInfo.filter { case (_, fi: FieldMetaData) => fi.fieldID.ubytes === ub }
    ms.foreach(ms => scribe.info(s" Field Info ${ByteUtils.ubytes2hex(ub)}: $ms"))
    if (ms.size > 1) BinCodecLibError(s"FieldMarker ${ub} found ${ms.size} times in fieldinfo.").asLeft
    else {
      ms.headOption.map(_._2).toRight(BinCodecLibError(s"FieldMarker ${ub} not found in fieldinfo."))
    }
  }

  private def getMapEntry[T, V](map: Map[T, V], key: T): Either[BCLibErr, V] = {
    map.get(key).toRight(BinCodecLibError(s" $key not found in map"))
  }

}

object DefinitionData {
  val pathSetAnother  = RawValue(List(UByte(0xFF))) //  FF indicates another path follows
  val pathSetEnd      = RawValue(List(UByte(0x00))) // 00 indicates the end of the PathSet
  val objectEndMarker = RawValue(List(UByte(0xE1))) // 0xE1, this is STObject not blob
  val arrayEndMarker  = RawValue(List(UByte(0xF1)))

  val objDel: List[UByte] = List(UByte(15))
  val arrDel: List[UByte] = List(UByte(14))

  final val objectMarkerEndName: String = "ObjectEndMarker"
  final val arrayMarkerEndName: String  = "ArrayEndMarker"

  implicit val showFieldInfo: Show[FieldMetaData] = Show[FieldMetaData] { fi =>
    s"${fi.fieldID.toHex} : ${fi.name} ${fi.datatype.name} : VL ${fi.isVLEncoded} " +
      s" Serialized/Signing ${fi.isSerialized}/${fi.isSigningField} "
  }
  implicit val show: Show[DefinitionData] = Show[DefinitionData] { dd =>
    val sortedFields: String =
      dd.fieldsInfo.values.toList
        .sortBy(e => (e.fieldID.toHex.length, e.fieldID.toHex))
        .map(info => info.show)
        .mkString("\n")

    sortedFields

  }
}
