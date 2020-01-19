package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import scodec.bits.ByteVector
import spire.math.{UByte, UInt}

import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.{BCLibErr, RawValue, BinCodecLibError}

/**
  *
  * Holds information about the datatype
  *
  * @param name  Name of the "type" of the field
  * @param value used as ordinal in javascript to order the fields in an object
  *  @param encoded UInt16 endoded bits of the value. Negative numbers will be wrapped (this is probably broken)
  *
  */
case class RippleDataType(name: String, value: Long, encoded: ByteVector)

/** For Ledger, Transaction, and Transaction Entry Types. Encoded as UINT16 but values vary */
case class MnemonicType(name: String, value: Long, encoded: ByteVector)

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

  import scodec.bits.BitVector

  val fieldID: BitVector = FieldMetaData.encodeFieldID(nth.toInt, datatype.value.toInt)
  val sortKey: UInt      = UInt(datatype.value) << 16 | UInt(nth)

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

  import scodec.bits.BitVector

  /*
   * Encodes the field id marker by field code and type code.
   * https://developers.ripple.com/serialization.html#field-ids
   * 1, 2, or 3 bytes result
   */
  def encodeFieldID(fName: Int, fType: Int): BitVector = {
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
    import scodec.codecs._
    import com.odenzo.scodec.spire._
    packed.map(subyte.encode(_).require).reduce(_ ++ _)
  }

}

case class DefinitionData(
    fieldByName: Map[String, FieldMetaData],
    fieldByMarker: Map[ByteVector, FieldMetaData],
    dataTypes: Map[String, RippleDataType],
    ledgerTypes: Map[String, MnemonicType],
    txnTypes: Map[String, MnemonicType],
    txnResultTypes: Map[String, MnemonicType]
) {

  /** Tries to find the fieldnInfo for the fieldName. This may not exist (e.g. hash)
    * because the field is not (isSerialized or isSigning)
    */
  def optFieldData(fieldName: String, fieldValue: Json): Option[FieldData] = {
    findFieldInfoByName(fieldName).map(fi => FieldData(fieldValue, fi))
  }

  def getFieldsByNth(nth: Long): Iterable[FieldMetaData] = {
    fieldByName.filter { case (_: String, fi: FieldMetaData) => fi.nth === nth }.values
  }

  def getFieldData(fieldName: String, fieldValue: Json): Either[BCLibErr, FieldData] = {
    Either.fromOption(optFieldData(fieldName, fieldValue), BinCodecLibError(s"$fieldName not found"))
  }

  def getFieldInfoByName(name: String): Either[BCLibErr, FieldMetaData] = getMapEntry(fieldByName, name)

  def findFieldInfoByName(fieldName: String): Option[FieldMetaData] = fieldByName.get(fieldName)

  def getDataType(name: String): Either[BCLibErr, RippleDataType]             = getMapEntry(dataTypes, name)
  def getTransactionTypeMnemonic(txn: String): Either[BCLibErr, MnemonicType] = getMapEntry(txnTypes, txn)
  def getLedgerEntryMnemonic(lt: String): Either[BCLibErr, MnemonicType]      = getMapEntry(ledgerTypes, lt)
  def getTxnResultMnemonic(lt: String): Either[BCLibErr, MnemonicType]        = getMapEntry(txnResultTypes, lt)

  /** Each field has a marker. Pre-filtered had duplicate markers (context dependant) */
  def findFieldByMarker(ub: ByteVector): Either[BCLibErr, FieldMetaData] = getMapEntry(fieldByMarker, ub)

  private def getMapEntry[T, V](map: Map[T, V], key: T): Either[BCLibErr, V] = {
    map.get(key).toRight(BinCodecLibError(s" $key not found in map"))
  }

}

object DefinitionData {

  import scodec.bits._
  val pathSetAnother  = hex"FF" // indicates another path follows
  val pathSetEnd      = hex"00" // indicates the end of the PathSet
  val objDel          = hex"0F" // Object Delimeter in some packed fields forget which
  val objectEndMarker = hex"E1" // indicates end of object this is STObject not blob
  val arrDel          = hex"0D" // Array delimeter
  val arrayEndMarker  = hex"F1" // End of Array

  final val objectMarkerEndName: String = "ObjectEndMarker"
  final val arrayMarkerEndName: String  = "ArrayEndMarker"

  implicit val showFieldInfo: Show[FieldMetaData] = Show[FieldMetaData] { fi =>
    s"${fi.fieldID.toHex} : ${fi.name} ${fi.datatype.name} : VL ${fi.isVLEncoded} " +
      s" Serialized/Signing ${fi.isSerialized}/${fi.isSigningField} "
  }
  implicit val show: Show[DefinitionData] = Show[DefinitionData] { dd =>
    val sortedFields: String =
      dd.fieldByName.values.toList
        .sortBy(e => (e.fieldID.toHex.length, e.fieldID.toHex))
        .map(info => info.show)
        .mkString("\n")

    sortedFields

  }
}
