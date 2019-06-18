package com.odenzo.ripple.bincodec

import io.circe.Decoder.Result
import io.circe.Json
import org.scalatest.FunSuite
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.codecs.MoneyCodecs
import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions, FieldInfo, RippleDataType}
import com.odenzo.ripple.bincodec.encoding.{BinarySerializer, CodecUtils, TypeSerializers}
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.utils.{ByteUtils, CirceCodecUtils, JsonUtils}

class TypeSerializersTest extends FunSuite with OTestSpec with CodecUtils {

  val defdata: DefinitionData = Definitions.fieldData

  val inorder = """
                  |{
                  | "TransactionType": "EscrowCancel"   ,
                  | "OfferSequence": 25   ,
                  | "Account": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"    ,
                  | "Owner": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"
                  |}
  """.stripMargin

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

    RippleCodecError.dump(res).foreach(e ⇒ logger.error(s"Trouble Parsing Sample JSON $e \n===\n${sample}\n===\n"))
    res.right.value
  }

  val bin = "1200042019000000198114EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA8214EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA"

  def encodeSingle(fieldName: String, data: Json) = {
    val req = dd.getFieldData(fieldName, data)
    req.foreach(v ⇒ logger.info(s"encoding Single Field: $v"))
    val ans = req.flatMap(TypeSerializers.encodeFieldAndValue(_, isNestedObject = false, false))
    RippleCodecError.dump(ans).foreach(e ⇒ logger.error(s"Trouble Encoding Field $fieldName $e "))
    ans
  }

  test("TransactionType") {
    // 12 → 0004
    val code = defdata.getTransactionType("EscrowCancel")
    logger.debug(s"TxnCode $code")
    val c = code.right.value
  }

  test("UInt32") {
    val sequence: Int                 = 25
    val v                             = Json.fromInt(sequence)
    val res: Encoded = getOrLog(TypeSerializers.encodeUIntN(v, "UInt32"))
    val hex                           = res.toHex
    logger.info(s"Result: $hex")
  }

  def encodeField(name: String) = {
    val fi: FieldInfo                     = getOrLog(defdata.getFieldInfo(name))
    val fieldId: Encoded = fi.fieldID
    logger.debug(s"$name => ${fieldId.toHex}")
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

    val munger: ACursor ⇒ ACursor                = CirceCodecUtils.prepareJsonObject(CirceCodecUtils.fieldNameChangeEx("type", "tipe"))
    implicit val decoder: Decoder[FieldTestData] = deriveDecoder[FieldTestData].prepare(munger)
  }

  test("Field Names") {
    val json            = loadJsonResource("/test/fixtures/data-driven-tests.json").right.value
    val arr: List[Json] = json.asObject.flatMap(_("fields_tests")).flatMap(_.asArray).map(_.toList).get
    logger.debug(s"Things to Test ${arr.length}")
    val ftd: Result[List[FieldTestData]] = arr.traverse(_.as[FieldTestData])
    ftd.left.foreach(d ⇒ logger.error(s"Decoding Failure: $d"))
    ftd.map { lst ⇒
      lst.foreach { fix: FieldTestData ⇒
        logger.debug(s"FieldTestData $fix")
        val res: List[UByte] = FieldInfo.encodeFieldID(fix.nth_of_type, fix.tipe)
        val hex              = res.map(ByteUtils.ubyte2hex).mkString
        logger.info("Result: " + hex)
        if (fix.name == "TickSize") hex shouldEqual ("0" + fix.expected_hex)
        else hex shouldEqual fix.expected_hex
      }
    }

  }

  test("Full Range of Field / Type Encoding") {
    defdata.fieldsData.values.foreach { ft ⇒
      val rt: RippleDataType = getOrLog(defdata.getTypeObj(ft.tipe))

      logger.debug(s"Field Type: $ft")
      logger.debug(s"Type      : $rt")
      // Special cases, including Amount?
      if (ft.isSigningField || ft.isSerialized) {
        val res = FieldInfo.encodeFieldID(ft.nth.toInt, rt.value.toInt)
      }

    // THe odballs include: rt.value >= 0 && rt.value < 1000 && ft.nth != 258 &&
    }

  }

  test("Fiat Amount") {
    MoneyCodecs.rippleEncodingOfFiatAmount(BigDecimal("0.00000000000001234500"))
  }

  test("XRP") {

    val min: ULong = ULong(0)
    val max: ULong = ULong.fromBigInt(spire.math.pow(BigInt(10), BigInt(17)))
    logger.info(s"Min - Max $min $max")

    t(max)
    t(max - ULong(1))
    t(ULong(370000000))

    def t(v: ULong): String = {
      val jsonV: String = v.toString()
      val json: Json    = Json.fromString(jsonV)
      logger.info(s"From $v Sending JSON: ${json.noSpaces}")
      val res: Encoded = getOrLog(MoneyCodecs.encodeXrpAmount(json))
      val hex                           = res.toHex
      logger.debug(s"$v  => $hex")
      hex
    }
  }

  test("XRP Encode") {
    val xrp: Json                     = Json.fromString("10000")
    val res: Encoded = getOrLog(MoneyCodecs.encodeXrpAmount(xrp))
    logger.info(s"XRP ${xrp.noSpaces}  => ${res.toHex}")
  }
}
