package com.odenzo.ripple.jsfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.serializing.DebuggingShows._
import com.odenzo.ripple.bincodec.serializing.{BinarySerializer, ContainerFields}
import com.odenzo.ripple.bincodec.utils.CirceUtils
import com.odenzo.ripple.bincodec.{OTestSpec, OTestUtils, TestFixData}

class CodecFixtures$Test extends FunSuite with OTestSpec with OTestUtils {

  test("Codec-Fixtures") {
    val fixturesAttempt      = super.loadJsonResource("/test/fixtures/codec-fixtures.json")
    val codec_fixtures: Json = getOrLog(fixturesAttempt, "Loading File")

    val dobj: JsonObject   = codec_fixtures.asObject.get
    val accountState: Json = dobj("accountState").get
    val transactions: Json = dobj("transactions").get
    val ledgerData         = dobj("ledgerData").get
    val accounts           = CirceUtils.decode(accountState, Decoder[List[TestFixData]])
    val txn                = CirceUtils.decode(transactions, Decoder[List[TestFixData]])
    val ledger             = CirceUtils.decode(ledgerData, Decoder[List[TestFixData]])

    val letsGo: List[TestFixData] = accounts.right.value ::: txn.right.value ::: ledger.right.value

    val badTests = Seq(
      262 // Appears to be testsing a ledger or something with field account_hash which is not in definitions
    )
    logger.info(s"TOTAL Number of Fixtures ${letsGo.length}")
    logger.info("First One: " + letsGo.head.toString)
    letsGo.zipWithIndex
      .drop(0)
      .filterNot(v ⇒ badTests.contains(v._2))
      .foreach { d ⇒
        logger.info(s"\n\n\n*************** DOING  FIXTURE ${d._2}\n")
        val fix              = d._1
        val json: JsonObject = fix.json
        val expected: String = fix.binary
        oneFixture(json, expected)
      }

  }

  def oneFixture(json: JsonObject, expected: String): Unit = {
    logger.info(s"OneFixture: \n ${json.asJson.spaces4}")
    logger.info(s"Expecting: $expected")
    val fields: BinarySerializer.NestedEncodedValues =
      getOrLog(ContainerFields.encodeSTObject(json.asJson, isSigning = false, isNested = true))

    logger.info(s"Field Order: ${fields.show}")
//
//
//    val dump = byField.map { case (fd, hex) ⇒ "Field: %s \nGot: %s".format(fd, hex) }.mkString("\n\t", "\n\t", "\n\n")
//    logger.info(s"All Fields SUmmary: $dump")
//
//    // Now we have computed ok -- lets check each field is correct
//    var offset = 0 // cough...
//    byField.zipWithIndex.foreach {
//      case ((fd, hex), indx) ⇒
//        logger.info(s"Checking FIeld Inex $indx $fd")
//        val fieldExpected = expected.slice(offset, offset + hex.length)
//        offset += hex.length
//        logger.debug(s"Field Expected vs Got: \n $fieldExpected \n $hex")
//        hex shouldEqual fieldExpected
//    }
//
//    // Now we can check the concatenation just for the hell of it
//    val fullHex: String = byField.map(_._2).mkString
//    fullHex shouldEqual expected
//
//    // But really should run through TopLevel
//    val stobjHex =
//      getOrLog(TypeSerializers.encodeSTObject(json.asJson, isNested = false,false).map(ByteUtils.ubytes2Hex),
//               "STObject")
//    stobjHex shouldEqual expected

  }
}
