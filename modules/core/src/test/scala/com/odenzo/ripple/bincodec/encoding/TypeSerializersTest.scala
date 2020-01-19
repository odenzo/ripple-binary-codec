package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import io.circe.literal._
import spire.math.ULong
import scodec.bits._

import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.codecs.MoneyCodecs
import com.odenzo.ripple.bincodec.reference._
import com.odenzo.ripple.bincodec.utils.JsonUtils

class TypeSerializersTest extends OTestSpec with CodecUtils {

  import scodec.bits.ByteVector

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
    json"""
      {
       "Owner": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2",
       "Account": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"    ,
       "TransactionType": "EscrowCancel"   ,
      "OfferSequence": 25
      }
    """

  val bin = hex"1200042019000000198114EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA8214EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA"

  def encodeSingle(fieldName: String, data: Json): Either[BinCodecLibError, ByteVector] = {
    val req = dd.getFieldData(fieldName, data)
    req.foreach(v => scribe.info(s"encoding Single Field: $v"))
    req
      .flatMap(TypeSerializers.encodeFieldAndValue(_, false))
      .leftMap(_.addMsg(s"Decoding Field $fieldName"))

  }

  test("TransactionType") {
    // 12 → 0004
    val code = getOrLog(defdata.getTransactionTypeMnemonic("EscrowCancel"))
    scribe.debug(s"TxnCode $code")

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
      val res: ByteVector = getOrLog(MoneyCodecs.encodeXrpAmount(json))
      val hex             = res.toHex
      scribe.debug(s"$v  => $hex")
      hex
    }
  }

  test("XRP Encode") {
    val xrp = json"10000"
    val res = getOrLog(MoneyCodecs.encodeXrpAmount(xrp))
    scribe.info(s"XRP ${xrp.noSpaces}  => ${res.toHex}")
  }

}
