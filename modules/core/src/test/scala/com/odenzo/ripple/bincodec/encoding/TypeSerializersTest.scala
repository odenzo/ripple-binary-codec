package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import io.circe.literal._
import spire.math.ULong

import com.odenzo.ripple.bincodec.{EncodedField, OTestSpec, BinCodecLibError, Encoded}
import com.odenzo.ripple.bincodec.codecs.{MoneyCodecs, UIntCodecs}
import com.odenzo.ripple.bincodec.reference._
import com.odenzo.ripple.bincodec.utils.JsonUtils

class TypeSerializersTest extends OTestSpec with CodecUtils {

  val defdata: DefinitionData = Definitions.fieldData

  val inorder = json"""
                  {
                   "TransactionType": "EscrowCancel"   ,
                  "OfferSequence": 25   ,
                   "Account": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"    ,
                   "Owner": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"
                  }
  """

  val sample =
    """
      |{
      | "Owner": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2",
      | "Account": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"    ,
      | "TransactionType": "EscrowCancel"   ,
      | "OfferSequence": 25                               
      |}
    """.stripMargin

  val json: Json = {
    val res = JsonUtils.parseAsJson(sample)
    getOrLog(res)
  }

  val bin = "1200042019000000198114EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA8214EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA"

  def encodeSingle(fieldName: String, data: Json): Either[BinCodecLibError, EncodedField] = {
    val req = dd.getFieldData(fieldName, data)
    req.foreach(v => scribe.info(s"encoding Single Field: $v"))
    req
      .flatMap(TypeSerializers.encodeFieldAndValue(_, false))
      .leftMap(_.addMsg(s"Decoding Field $fieldName"))

  }

  test("TransactionType") {
    // 12 → 0004
    val code = getOrLog(defdata.getTransactionType("EscrowCancel"))
    scribe.debug(s"TxnCode $code")

  }

  test("UInt32") {
    val sequence: Int = 25
    val v             = Json.fromInt(sequence)
    val res: Encoded  = getOrLog(UIntCodecs.encodeUIntN(v, "UInt32"))
    val hex           = res.toHex
    scribe.info(s"Result: $hex")
  }

  def encodeField(name: String): Unit = {
    val fi: FieldMetaData = getOrLog(defdata.getFieldInfo(name))
    val fieldId: Encoded  = fi.fieldID
    scribe.debug(s"$name => ${fieldId.toHex}")
  }

  test("Sample Fields") {

    encodeField("TransactionType")
    encodeField("OfferSequence")
    encodeField("Account")
    encodeField("Owner")

  }

  import cats.implicits._
  import io.circe._
  import io.circe.generic.semiauto._
  case class FieldTestData(type_name: String, name: String, nth_of_type: Int, tipe: Int, expected_hex: String)

  object FieldTestData {

    val munger: ACursor => ACursor =
      prepareJsonObject(fieldNameChangeEx("type", "tipe"))
    implicit val decoder: Decoder[FieldTestData] = deriveDecoder[FieldTestData].prepare(munger)
  }

  test("XRP") {

    val min: ULong = ULong(0)
    val max: ULong = ULong.fromBigInt(spire.math.pow(BigInt(10), BigInt(17)))
    scribe.info(s"Min - Max $min $max")

    t(max)
    t(max - ULong(1))
    t(ULong(370000000))

    def t(v: ULong): String = {
      val jsonV: String = v.toString()
      val json: Json    = Json.fromString(jsonV)
      scribe.info(s"From $v Sending JSON: ${json.noSpaces}")
      val res: Encoded = getOrLog(MoneyCodecs.encodeXrpAmount(json))
      val hex          = res.toHex
      scribe.debug(s"$v  => $hex")
      hex
    }
  }

  test("XRP Encode") {
    val xrp: Json    = Json.fromString("10000")
    val res: Encoded = getOrLog(MoneyCodecs.encodeXrpAmount(xrp))
    scribe.info(s"XRP ${xrp.noSpaces}  => ${res.toHex}")
  }

}
