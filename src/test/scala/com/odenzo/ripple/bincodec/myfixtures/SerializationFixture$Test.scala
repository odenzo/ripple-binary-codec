package com.odenzo.ripple.bincodec.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.FunSuite
import spire.math.UByte

import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.utils.{ByteUtils, FixtureUtils, RippleBase58}
import com.odenzo.ripple.bincodec.{Encoded, EncodedField, EncodedNestedVals, OTestSpec}

/** This test is designed to process Transaction Request and Response files */
class SerializationFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils {


  val mixed: List[(JsonObject, JsonObject)] = loadTransactions("/mytests/all_txns.json")
  val allTxn =  mixed

  /** See if we can get the correct Signing Public Key and Hash to start */
  def testJustSigningSerialization(rq: JsonObject, rs: JsonObject) = {

    logger.info(s"Request: ${rq.asJson.spaces4}")
    logger.info(s"Response: ${rs.asJson.spaces4}")


    // This is from response which is populated with default values.
    val result         = findRequiredObject("result", rs)
    val txjson         = findRequiredObject("tx_json", result)
    val txblob: String = findRequiredStringField("tx_blob", result)

    // We need to sign the response tx_json filled in.
    // BinarySerializerPublic.sign

    val encoded: EncodedNestedVals = getOrLog(TypeSerializers.encodeTopLevel(txjson.asJson, isSigning = false))
    val genTxBlob                    = encoded.toHex

    // Lets go check and see that all the encoded fields are actually in TxBlob, won't be complete though
    encoded.enclosed.foreach {
      case fe @ EncodedField(fieldValue: Encoded, data) =>
        logger.info(s"Examinging Field ${data.key}")
        val hex = fe.toHex
        logger.info(s"${txblob.contains(hex)} : $hex")
      case other => logger.info(s"Skipping $other")
    }

    // Blob has F9F1 at the end, and a few extra fields it seems.

    genTxBlob shouldEqual txblob

    val ubytes: List[UByte] = encoded.encoded.map(_.rawBytes).foldLeft(List.empty[UByte])(_ ::: _)
    val bytes               = ubytes.map(_.toByte)

    encoded.asRight
  }

  test("ALL") {
    val done =
      allTxn.traverse(v ⇒ testJustSigningSerialization(v._1, v._2))

    val ok: List[EncodedNestedVals] = getOrLog(done)

  }

  def dumpNestedFieldsInfo(nested: EncodedNestedVals): Unit = {
    import com.odenzo.ripple.bincodec.syntax.debugging._
    logger.info(s"The tree: ${nested.show}")
  }

  test("Specific Cases") {
    allTxn
      .drop(1)
      .take(1)
      .traverse(v ⇒ testJustSigningSerialization(v._1, v._2))
  }

}
