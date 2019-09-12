package com.odenzo.ripple.bincodec.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.utils.{ByteUtils, FixtureUtils}
import com.odenzo.ripple.bincodec.{EncodedSTObject, OTestSpec, RippleCodecAPI, TestLoggingConfig}
import io.circe.optics._
import io.circe.optics.JsonPath._

/** This test is designed to process Transaction Request and Response files */
class SerializationFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils {

  import com.odenzo.ripple.bincodec.syntax.showinstances._

  val mixed: List[(JsonObject, JsonObject)] = loadTransactions("/mytests/all_txns.json")
  val allTxn                                = mixed

  def dumpNestedFieldsInfo(nested: EncodedSTObject): Unit = {

    scribe.info(s"The tree: ${nested.show}")
  }

  test("ALL") {
    TestLoggingConfig.debugLevel()
    val done                      = allTxn.traverse(v => testViaAPI(v._1, v._2))
    val ok: List[EncodedSTObject] = getOrLog(done)
  }

  /** See if we can get the correct Signing Public Key and Hash to start */
  private def testViaAPI(rq: JsonObject, rs: JsonObject): Either[Nothing, EncodedSTObject] = {

    scribe.debug(s"Request: ${rq.asJson.spaces4}")
    scribe.debug(s"Response: ${rs.asJson.spaces4}")

    // This is from response which is populated with default values.

    val result         = findRequiredObject("result", rs)
    val txjson         = findRequiredObject("tx_json", result)
    val txblob: String = findRequiredStringField("tx_blob", result)

    // We need to sign the response tx_json filled in.
    // BinarySerializerPublic.sign

    //val encoded: EncodedNestedVals = getOrLog(TypeSerializers.encodeTopLevel(txjson.asJson, isSigning = false))
    val encoded               = getOrLog(RippleCodecAPI.binarySerialize(txjson))
    val objBytes: Array[Byte] = getOrLog(RippleCodecAPI.serializedTxBlob(txjson))
    val genTxBlob             = encoded.toHex

    objBytes shouldEqual encoded.toBytes

    if (encoded.toHex =!= txblob) {
      import com.odenzo.ripple.bincodec.syntax
      logger.warn(s"Encoded: ${encoded.show}")
      logger.warn(s"MisMatch:\n ${encoded.toHex} \n ${txblob}")
    }
    encoded.toHex shouldEqual txblob

    val signedBytes: Array[Byte]       = getOrLog(RippleCodecAPI.signingTxBlob(txjson))
    val signedStrBytes: Array[Byte]    = getOrLog(RippleCodecAPI.signingTxBlob(txjson.asJson.noSpaces))
    val signedEncoded: EncodedSTObject = getOrLog(RippleCodecAPI.binarySerializeForSigning(txjson))

    signedBytes shouldEqual signedStrBytes
    signedEncoded.toBytes shouldEqual signedBytes

    // Nothing in message to compare without actually signing.

    encoded.asRight
  }

  test("Specific Cases") {
    allTxn
      .drop(1)
      .take(1)
      .traverse(v => testViaAPI(v._1, v._2))
  }

}
