package com.odenzo.ripple.bincodec.serializing

import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe._
import io.circe.syntax._
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.reference.{DefinitionData, RippleType}
import com.odenzo.ripple.bincodec.serializing.BinarySerializer.{Encoded, FieldData, FieldEncoded, NestedEncodedValues, RawEncodedValue, RippleTypeEncoded}
import com.odenzo.ripple.bincodec.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{AppError, AppException, OError}
import com.odenzo.ripple.bincodec.utils.{ByteUtils, CirceUtils, JsonUtils}

/**
  * I am not building these based on pure JSON rather
  * than the WSModel objects
  */
object TypeSerializers extends StrictLogging with JsonUtils with SerializerUtils {

  def encodeTopLevel(json: Json, isSigning: Boolean): Either[AppError, NestedEncodedValues] = {

    ContainerFields.encodeSTObject(json, isNested = false, isSigning = isSigning)
  }

  /**
    * Encodes a field and value. It seems we need to know if the field is in a nested JSObject
    * in order to not VL Encode Account="r...", e.g. in txJson
    * A field here by definition has a fieldName and associated meta data.
    *
    * Note that encoding a field may produce nested structure, working on tht.
    *
    * <p>
    *     Meh, need some conventions regarding VLEncoding structure and also
    *     when an incoder should place the FieldType bytes in front or not.
    *     There is no real need to do when being called from here.
    *     When passed in a FieldData it SHOULD NOT, as FieldEncoded will handle it.
    *     Values in FieldEncoded SHOULD NOT have field keys in them.
    *     But when we pass in Json and get RawEncoded it usually has it now.
    *     Problem is things like account that differs on nesting.
    *     So, lets simplify a bit more and see whats best in regards to VLEncoding and
    *     FieldType tag.
    *     Note that array and object end stuff always embedded in raw vlaue.
    * </p>
    * @param fieldData
    *
    * @return
    */
  def encodeFieldAndValue(fieldData: FieldData,
                          isNestedObject: Boolean,
                          signingModeOn: Boolean): Either[AppError, FieldEncoded] = {
    val fieldName: String = fieldData.key
    val fieldValue: Json  = fieldData.v

    logger.debug(s"Encoding FieldValue: $fieldData")

    val valueBytes: Either[AppError, Encoded] = fieldData.fi.fieldTypeName match {
      case "UInt16" if fieldName === "LedgerEntryType" ⇒ encodeLedgerEntryType(fieldValue)
      case "UInt16" if fieldName === "TransactionType" ⇒ encodeTransactionType(fieldValue)
      case "UInt8"                                     ⇒ encodeUIntN(fieldValue, "UInt8")
      case "UInt16"                                    ⇒ encodeUIntN(fieldValue, "UInt16")
      case "UInt32"                                    ⇒ encodeUIntN(fieldValue, "UInt32")
      case "UInt64"                                    ⇒ encodeUInt64(fieldValue)

      case "Hash160" ⇒ encodeHash160(fieldValue)
      case "Hash256" ⇒ encodeHash256(fieldValue)
      case "Blob"    ⇒ encodeBlob(fieldValue)

      case "Amount" ⇒ CurrencyEncoders.encodeAmount(fieldValue)

      case "PathSet"   ⇒ encodePathSet(fieldData)
      case "Vector256" ⇒ ContainerFields.encodeVector256(fieldData)
      case "STArray"   ⇒ ContainerFields.encodeSTArray(fieldData, signingModeOn)
      case "STObject"  ⇒ ContainerFields.encodeSTObject(fieldValue, isNestedObject, signingModeOn)

      case "AccountID" ⇒
        if (isNestedObject) AccountIdCodecs.encodeAccountNoVL(fieldValue)
        else AccountIdCodecs.encodeAccount(fieldValue)
      case other ⇒ AppError(s"Not handling Field Type $other").asLeft

    }

    // Lets drag some baggage along!
    val full: Either[AppError, FieldEncoded] = valueBytes.map(FieldEncoded(_, fieldData))
    full

  }

  /** These is really a container. Inside is a list of  datasteps and delimeters **/
  def encodePathSet(data: FieldData): Either[AppError, NestedEncodedValues] = {

    // Another array of arrays. List of PathSet, each PathSet has Paths, each Path has  PathSteps

    val pathList: Either[AppError, List[Json]] = json2array(data.v)


    val another =  DefinitionData.pathSetAnother
    val end     = DefinitionData.pathSetEnd

    val encodedPaths = pathList.flatMap { path: List[Json] ⇒
      path.traverse(encodePathStep)
    }

    // No for each of the paths we need to put in deliminers
    encodedPaths.map { listOfPaths: List[RawEncodedValue] ⇒
      val rest: List[RawEncodedValue] = listOfPaths.dropRight(1).flatMap(path ⇒ List(path, another))

      val lastPath: RawEncodedValue      = listOfPaths.takeRight(1).head
      val endList: List[RawEncodedValue] = List(lastPath, end)

      val subFields: List[RawEncodedValue] = rest ::: endList

      NestedEncodedValues(subFields)

    }
  }

  /** @json The array surrounding the object **/
  def encodePathStep(json: Json): Either[AppError, RawEncodedValue] = {
    /*
      account by itself
      currency by itself
      currency and issuer as long as the currency is not XRP
      issuer by itself
     */
    logger.debug(s"Encoding Path Step\n ${json.spaces2}")
    // Another array of arrays
    val arr: Either[AppError, JsonObject] = json2array(json).map(_.head).flatMap(json2object)

    // In a step the following fields are serialized in the order below
    // FIXME: Move to reference data
    val accountType: UByte       = UByte(1)
    val currencyType: UByte      = UByte(16)
    val issuerType: UByte        = UByte(32)
    val currencyAndIssuer: UByte = currencyType | issuerType

    def combine2(amtType: UByte, amt: RawEncodedValue): RawEncodedValue = {
      RawEncodedValue(amtType +: amt.ubytes)
    }

    /** Can we make these more speficic. Issuer is AccountID, any type is Amount **/
    def combine3(amtType: UByte, curr: RawEncodedValue, issuer: RawEncodedValue): RawEncodedValue = {
      RawEncodedValue(amtType +: (curr.ubytes ++ issuer.ubytes))
    }
    val ans = arr.flatMap { obj: JsonObject ⇒
      logger.debug(s"JOBJ: ${obj.asJson.spaces2}")

      val fields: Seq[Option[Json]] = Seq("account", "currency", "issuer").map(k ⇒ obj(k))
      fields match {
        case Seq(Some(account), None, None) ⇒
          AccountIdCodecs.encodeAccountNoVL(account).map(ac ⇒ combine2(accountType, ac))

        case Seq(None, Some(curr), None) ⇒
          CurrencyEncoders.encodeCurrency(curr).map(combine2(currencyType, _))

        case Seq(None, None, Some(issuer)) ⇒
          AccountIdCodecs.encodeAccountNoVL(issuer).map(combine2(issuerType, _))

        case Seq(None, Some(curr), Some(issuer)) ⇒
          // TODO: Validate currency is not XRP , with special currency encoding TBC
          (CurrencyEncoders.encodeCurrency(curr), AccountIdCodecs.encodeAccountNoVL(issuer))
            .mapN(combine3(currencyAndIssuer, _, _))

      }
    }

    ans

  }

  /** Encodes the hex including the Variable Length info */
  def encodeBlob(json: Json): Either[AppError, RawEncodedValue] = {
    for {
      str    <- CirceUtils.decode(json, Decoder[String])
      ubytes ← ByteUtils.hex2ubytes(str)
      rev    ← VLEncoding.prependVL(ubytes)
    } yield rev

  }

  def encodeHash(json: Json, byteLen: Int): Either[AppError, RawEncodedValue] = {
    // This looks like Hex Already... in fact just round tripping
    val str: ErrorOr[String] = CirceUtils.decode(json, Decoder[String])
    val ans: Either[AppError, List[UByte]] = str.flatMap { v ⇒
      ByteUtils.hex2ubytes(v)
    }
    val checked = ans.flatMap(ByteUtils.ensureMaxLength(_, byteLen))
    checked.map(ByteUtils.zeroPadBytes(_, byteLen))
    ans.fmap(RawEncodedValue)
  }

  def encodeHash160(json: Json): Either[AppError, RippleTypeEncoded] = {
    val rtype: Either[OError, RippleType]          = dd.getTypeObj("Hash160")
    val encoded: Either[AppError, RawEncodedValue] = encodeHash(json, 20)

    (encoded, rtype).mapN(RippleTypeEncoded)
  }

  def encodeHash256(json: Json): Either[AppError, RippleTypeEncoded] = {
    val rtype: Either[OError, RippleType]          = dd.getTypeObj("Hash256")
    val encoded: Either[AppError, RawEncodedValue] = encodeHash(json, 32)

    (encoded, rtype).mapN(RippleTypeEncoded(_, _))
  }

  def encodeTransactionType(json: Json): Either[AppError, RawEncodedValue] = {
    val res: Either[AppError, RawEncodedValue] = CirceUtils.decode(json, Decoder[String]).flatMap(v ⇒ txnType2Bin(v))
    res
  }

  def encodeLedgerEntryType(json: Json): Either[AppError, RawEncodedValue] = {
    val res: Either[AppError, RawEncodedValue] = CirceUtils.decode(json, Decoder[String]).flatMap(v ⇒ ledgerType2Bin(v))
    res
  }

  def encodeTxnResultType(json:Json): Either[AppError, RawEncodedValue] = {
    json2string(json).flatMap(txnResultType2Bin)
  }

  /** Adapted from the Javascript
    * Redo with one UInt Decoder and a function for each type.
    * */
  def encodeUIntN(v: Json, dataType: String): Either[AppError, RawEncodedValue] = {
    AppException.wrap(s"Binary Encoding JSON # $v") {

      val number: Option[ULong] = for {
        numeric <- v.asNumber
        ulong   <- numeric.toLong.map(ULong(_))
      } yield ulong

      val unsigned = Either.fromOption(number, AppError(s"JSON ${v.spaces2} was not a Unisgned Long Number"))

      val ans: Either[OError, RawEncodedValue] = unsigned.flatMap(v ⇒ encodeULong(v, dataType))
      ans
    }
  }



  def encodeUInt64(json: Json): Either[AppError, RawEncodedValue] = {
    parseUInt64(json).flatMap(encodeULong(_, "UInt64"))
  }

  /** TODO: Potentially bugger, check thus esp UInt64 */
  def encodeULong(value: ULong, dataType: String): Either[Nothing, RawEncodedValue] = {
    val fieldLength = dataType match {
      case "UInt8"  ⇒ 1
      case "UInt16" ⇒ 2
      case "UInt32" ⇒ 4
      case "UInt64" ⇒ 8
      case other    ⇒ throw new IllegalArgumentException(s"$other was not a valid unsigned int type")
    }

    val bytes: Either[Nothing, List[UByte]] = (0 until fieldLength)
      .map { i: Int ⇒
        val calcUnsigned: ULong = value >>> (i * 8)
        UByte(calcUnsigned.toByte)
      }
      .toList
      .reverse
      .asRight
    bytes.foreach(bl ⇒ assert(bl.length == fieldLength))
    bytes.map(RawEncodedValue)
  }

  /** Given a transaction type name, an enumeration basically, return its value. -1 invalid until 101 Error if not
    * found. I think these are always UInt16 incoded.
    * */
  def txnType2Bin(txnName: String): Either[AppError, RawEncodedValue] = {
    dd.getTransactionType(txnName).flatMap(l ⇒ encodeUIntN(Json.fromLong(l), "UInt16"))

  }

  def ledgerType2Bin(entryType: String): Either[AppError, RawEncodedValue] = {
    dd.getLedgerEntryType(entryType).flatMap(l ⇒ encodeUIntN(Json.fromLong(l), "UInt16"))
  }

  // This should be pre-baked of course.
  def txnResultType2Bin(entryType: String): Either[AppError, RawEncodedValue] = {
    dd.getTransactionType(entryType).flatMap(l ⇒ encodeUIntN(Json.fromLong(l), "UInt16"))
  }
}
