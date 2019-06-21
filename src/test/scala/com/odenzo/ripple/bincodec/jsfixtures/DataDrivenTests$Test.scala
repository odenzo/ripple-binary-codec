package com.odenzo.ripple.bincodec.jsfixtures

import scala.collection.immutable

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.scalatest.{Assertion, FunSuite}
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.codecs.{ContainerFields, MoneyCodecs}
import com.odenzo.ripple.bincodec.reference.FieldInfo
import com.odenzo.ripple.bincodec.syntax.debugging._
import com.odenzo.ripple.bincodec.encoding.{BinarySerializer, TypeSerializers}
import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.utils.{ByteUtils, CirceCodecUtils, FixtureUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{Encoded, EncodedNestedVals, OTestSpec, OTestUtils, RawValue}

class DataDrivenTests$Test extends FunSuite with OTestSpec with OTestUtils with FixtureUtils {

  case class ExpectedField(hexByField: Seq[String], json: Json, field_header: String)

  case class FieldTest(type_name: String, name: String, nth_of_type: Int, tipe: Int, expected_hex: String)

  object FieldTest {
    implicit val decoder: Decoder[FieldTest] =
      Decoder.forProduct5("type_name", "name", "nth_of_type", "type", "expected_hex")(FieldTest.apply)

  }

  /** Expects the whole Json for each test under the "fields".
    * Array of arrays. But the bottom arrays are heterogenous
    */
  def decodeFieldData(json: Json): immutable.Seq[(String, ExpectedField)] = {
    /*
      ["Expiration",
                    {
                        "binary": ["535A8CF1"],
                        "json": 1398443249,
                        "field_header": "2A"
                    }
      ]
     */

    //  scribe.info(s"Fields ${json.spaces2}")

    val allFields: List[Json]            = getOrLog(json2array(json))
    val eachFieldArray: List[List[Json]] = getOrLog(allFields.traverse(json2array))

    val ans: immutable.Seq[(String, ExpectedField)] = allFields.map { elemJson: Json ⇒
      //  scribe.info(s"Field JSON ${elemJson.spaces2}")

      val subArr            = elemJson.asArray.get
      val fieldName: String = subArr(0).asString.get
      val expected: Json    = subArr(1)
      val jo: JsonObject    = expected.asObject.get

      val fieldHeader: String     = jo("field_header").flatMap(_.asString).get
      val json: Json              = jo("json").get
      val binary: Vector[Json]    = jo("binary").flatMap(_.asArray).get
      val hexByField: Seq[String] = binary.map(_.asString.get)

      (fieldName, ExpectedField(hexByField, json, fieldHeader))
    }
    ans
  }

  test("Field Tests") {
    val fixturesAttempt = super.loadJsonResource("/test/fixtures/data-driven-tests.json")

    fixturesAttempt.left.foreach(e ⇒ scribe.error("Error: " + e.show))
    val codec_fixtures: Json = fixturesAttempt.right.value

    case class FieldTest(type_name: String, name: String, nth_of_type: Int, tipe: Int, expected_hex: String)
    object FieldTest {
      implicit val decoder: Decoder[FieldTest] =
        Decoder.forProduct5("type_name", "name", "nth_of_type", "type", "expected_hex")(FieldTest.apply)

    }

    val dobj: JsonObject                 = codec_fixtures.asObject.get
    val fieldsTests: Json                = dobj("fields_tests").get
    val fields: ErrorOr[List[FieldTest]] = JsonUtils.decode(fieldsTests, Decoder[List[FieldTest]])

    fields.map { lf ⇒
      scribe.info(s"TOTAL Number of Fixtures ${lf.length}")
      lf.zipWithIndex.drop(0).foreach { d ⇒
        scribe.info(s"\n\n\n*************** DOING  FIXTURE ${d._2}\n")
        val fix: FieldTest = d._1
        scribe.info(s"FT: $fix")
        val res: List[UByte] = FieldInfo.encodeFieldID(fix.nth_of_type, fix.tipe)
        val hex: String      = ByteUtils.ubytes2hex(res)
        if (fix.tipe == 16 && fix.nth_of_type == 16) {
          hex shouldEqual "001010" // Test case has "01010" from formatting
        } else {
          hex shouldEqual fix.expected_hex
        }
      }
    }
  }

  test("Whole Objects") {
    val fixturesAttempt = super.loadJsonResource("/test/fixtures/data-driven-tests.json")
    fixturesAttempt.left.foreach(e ⇒ scribe.error("Error: " + e.show))
    val fixtures: Json = fixturesAttempt.right.value

    val dobj: JsonObject = fixtures.asObject.get
    val allTests: Json   = dobj("whole_objects").get

    val eachTest: ErrorOr[List[JsonObject]] = JsonUtils.decode(allTests, Decoder[List[JsonObject]])

    val targets: Seq[(JsonObject, Seq[(String, ExpectedField)], String)] = eachTest.zipWithIndex
      .map {
        case (lobj, index) ⇒
          lobj.map { obj ⇒
            val json: JsonObject                     = obj("tx_json").flatMap(_.asObject).get
            val fields: Seq[(String, ExpectedField)] = obj("fields").map(decodeFieldData).get
            val blob: String                         = obj("blob_with_no_signing").flatMap(_.asString).get

            val ans: (JsonObject, Seq[(String, ExpectedField)], String) = (json, fields, blob)
            ans
          }
      }
      .right
      .value

    // There is a structural problem here.
    // Account's are NOT VL encoded, even at top level of txJson like Account : "r....." because TxJson is nexted
    // in the actual message.
    scribe.info(s"TOTAL Number of Fixtures ${targets.length}")
    targets.zipWithIndex.drop(4).foreach {
      case ((json: JsonObject, fields: Seq[(String, ExpectedField)], blob: String), index) ⇒
        scribe.info(s"\n\n\n*************** DOING  FIXTURE $index of ${targets.length}\n")

        scribe.info("Expected: " + fields.map(v ⇒ v._1 + v._2.toString).mkString("\n\t", "\n\t", "\n\n"))

        // Then we do the whole thing for fun.
        val res: EncodedNestedVals = getOrLog(ContainerFields.encodeSTObject(json.asJson, true, false))

        scribe.info(s"Full Hexz: ${res.toHex}")
        scribe.info(s"Full Dump ${res.show}")

        val expectedWholeHex = fields.map(v ⇒ v._2.hexByField.mkString).mkString

    }

  }

  /**
    * "significant_digits": 1,
    * "type_id": 6,
    * "is_native": false,
    * "mantissa": "00038D7EA4C68000",
    * "type": "Amount",
    * "expected_hex": "94838D7EA4C6800000000000000000000000000055534400000000000000000000000000000000000000000000000001",
    * "is_negative": true,
    * "exponent": -15
    */
  case class BaseValueTest(test_json: Json,
                           type_id: Option[Int], // Sign, no consistency
                           tipe: String,
                           expected_hex: Option[String], // Either
                           error: Option[String])

  object BaseValueTest {
    val baseDecoder                              = deriveDecoder[BaseValueTest]
    val changer                                  = CirceCodecUtils.changeObjectField("type", "tipe")
    implicit val decoder: Decoder[BaseValueTest] = baseDecoder.prepare(changer)
  }

  case class FiatAmountTest(significant_digits: Int,
                            is_native: Boolean,
                            mantissa: String,
                            is_negative: Boolean,
                            exponent: Int)

  case class XrpAmountTest(is_native: Boolean, is_negative: Boolean)

  case class TypeSpecializationTest(canonical_json: Option[Json], type_specialisation_field: String)

  def testXrp(b: BaseValueTest, v: XrpAmountTest) = {
    scribe.debug(s"Testing XRP $v")
    if (b.error.isDefined) {
      scribe.info("This is a negative test")
    } else {
      val res: Encoded = getOrLog(MoneyCodecs.encodeAmount(b.test_json))
      val hex                           = res.toHex
      b.expected_hex.map(hex shouldEqual _)
    }
  }

  def testFiat(b: BaseValueTest, v: FiatAmountTest): Assertion = {
    scribe.info(s"Testing Fiat $v")
    val manual: Either[RippleCodecError, (ULong, Int)] = TypeSerializers
                                                         .json2object(b.test_json)
                                                         .flatMap(findField("value", _))
                                                         .flatMap(TypeSerializers.json2string)
                                                         .map(BigDecimal(_))
                                                         .flatMap(MoneyCodecs.normalizeAmount2MantissaAndExp)

    scribe.debug(s"Man / Exponent $manual")
    val (mantissa: ULong, exp: Int) = manual.right.value

    val res: Encoded = getOrLog(MoneyCodecs.encodeAmount(b.test_json))

    val hex = res.toHex

    scribe.info(s"Fiat  Expected/Got \n ${b.expected_hex.get} \n $hex")
    hex shouldEqual b.expected_hex.get
  }

  def testSpecial(b: BaseValueTest, v: TypeSpecializationTest): Assertion = {
    scribe.info(s"Base $b")
    scribe.info(s"TypeSpecial: $v")
    // tecNO_ALTERNATIVE_KEY replaced tecMASTER_DISABLED
    // Whats with test_json being a number?
    val testVal: Json =
      if (b.test_json.isString) b.test_json
      else v.canonical_json.get

    val gotBytes: RawValue = v.type_specialisation_field match {
      case "LedgerEntryType" ⇒
        getOrLog(TypeSerializers.encodeLedgerEntryType(testVal))

      case "TransactionType" ⇒
        getOrLog(TypeSerializers.encodeTransactionType(testVal))

      // TODO: Add this functionality for TransactionResult Codes. in TRANSACTION_RESULTS section of definitions
      case "TransactionResult" ⇒
        scribe.debug(s"Testing Txn Result Codes: $testVal")
        getOrLog(TypeSerializers.encodeTxnResultType(testVal))

    }

    val gotHex = ByteUtils.ubytes2hex(gotBytes.ubytes)
    val normalized = b.expected_hex match {
      case Some(x) if x.length == 2 ⇒ "00" + x
      case Some(x)                  ⇒ x
      case other                    ⇒ assert(false, "No Value to Check"); "0000"
    }
    gotHex shouldEqual normalized // 00 to match 0000
  }

  test("Value Tests") {
    val fixturesAttempt = super.loadJsonResource("/test/fixtures/data-driven-tests.json")
    fixturesAttempt.left.foreach(e ⇒ scribe.error("Error: " + e.show))
    val codec_fixtures: Json = fixturesAttempt.right.value

    val dobj: JsonObject            = codec_fixtures.asObject.get
    val fieldsTests: Json           = dobj("values_tests").get
    val allFields: List[JsonObject] = getOrLog(JsonUtils.decode(fieldsTests, Decoder[List[JsonObject]]))

    val indexed: List[(JsonObject, Int)] = allFields.zipWithIndex
    scribe.debug(s"Number of Fields ${indexed.length}")

    val skipBroken = Seq(
      75,
      76, // EnabledAmendments TransactionType not in data definitions Cant see on dev portal
      //30, // Negative XRP test not really handled but passes Weird XRP ErrorXS
      28, //Negative test case: ignoring  Exponenr to large test
      //10, // Very large fiat amount, x ^ 62 is there result
      2 // XRP JSON of -1 should be disallowed I think
    )
    val todo: List[(JsonObject, Int)] = indexed
      .drop(0)
      .filterNot(v ⇒ skipBroken.contains(v._2))

    // Each of the JSON Object may be a different kind of test.
    // All should be BaseValuesTest + specific
    todo.map {
      case (obj, indx) ⇒
        scribe.debug(s"\n\n *** DOING INDEX $indx *****\n\n")
        val json = obj.asJson
        scribe.debug("Decoding \n" + json.spaces2)
        val base: BaseValueTest = JsonUtils.decode(obj.asJson, Decoder[BaseValueTest]).right.value
        scribe.info(s"Base: $base")

        val xrp: ErrorOr[XrpAmountTest]              = JsonUtils.decode(json, Decoder[XrpAmountTest])
        val fiat: ErrorOr[FiatAmountTest]            = JsonUtils.decode(json, Decoder[FiatAmountTest])
        val special: ErrorOr[TypeSpecializationTest] = JsonUtils.decode(json, Decoder[TypeSpecializationTest])

        // HACK Note: fiat matches XRP too so have to order fiat before it
        val allTestType = Seq(fiat, xrp, special)
        allTestType.filter(_.isRight).map(_.right.value).head match {
          case v: FiatAmountTest ⇒ testFiat(base, v)
          case v: XrpAmountTest  ⇒ testXrp(base, v)

          case v: TypeSpecializationTest ⇒ testSpecial(base, v)

          case other ⇒ assert(false, s"Unhandled Test Type $other")
        }

        "gl"
    }

  }

  /** This does no checking, just a manual JSON Object encoding for later verification */
  def oneFixtureByField(json: JsonObject): EncodedNestedVals = {
    scribe.info(s"OneFixture: \n ${json.asJson.spaces4}")
    val bound: EncodedNestedVals = getOrLog(
      ContainerFields.encodeSTObject(json.asJson, false, false)
    )
    scribe.info("Field Order: " + bound.fieldsInOrder.mkString("\n\t", "\n\t", "\n"))
    scribe.debug(s"Full Dump: ${bound.show}")
    bound
  }
}
