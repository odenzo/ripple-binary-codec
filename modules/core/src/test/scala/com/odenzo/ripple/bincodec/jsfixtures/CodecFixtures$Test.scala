package com.odenzo.ripple.bincodec.jsfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.codecs.ContainerFields
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.{EncodedSTObject, OTestSpec, OTestUtils}

class CodecFixtures$Test extends FunSuite with OTestSpec with OTestUtils {

  test("Codec-Fixtures") {
    val fixturesAttempt      = super.loadJsonResource("/test/fixtures/codec-fixtures.json")
    val codec_fixtures: Json = getOrLog(fixturesAttempt, "Loading File")

    val dobj: JsonObject   = codec_fixtures.asObject.get
    val accountState: Json = dobj("accountState").get
    val transactions: Json = dobj("transactions").get
    val ledgerData         = dobj("ledgerData").get
    val accounts           = getOrLog(decode(accountState, Decoder[List[TestFixData]]))
    val txn                = getOrLog(decode(transactions, Decoder[List[TestFixData]]))
    val ledger             = getOrLog(decode(ledgerData, Decoder[List[TestFixData]]))

    val letsGo: List[TestFixData] = accounts ::: txn ::: ledger

    val badTests = Seq(
      262 // Appears to be testsing a ledger or something with field account_hash which is not in definitions
    )
    scribe.debug(s"TOTAL Number of Fixtures ${letsGo.length}")
    letsGo.zipWithIndex
      .drop(0)
      .filterNot(v => badTests.contains(v._2))
      .foreach { d =>
        scribe.info(s"\n\n\n*************** DOING  FIXTURE ${d._2}\n")
        val fix              = d._1
        val json: JsonObject = fix.json
        val expected: String = fix.binary
        oneFixture(json, expected)
      }

  }
  import com.odenzo.ripple.bincodec.syntax.compact._

  def oneFixture(json: JsonObject, expected: String): Unit = {
    scribe.info(s"OneFixture: \n ${json.asJson.spaces4}")
    scribe.info(s"Expecting: $expected")
    val fields: EncodedSTObject = getOrLog(
      ContainerFields.encodeSTObject(json.asJson, isSigning = false, isNested = true)
    )

    scribe.info(s"Field Order: ${fields.show}")

  }
}
