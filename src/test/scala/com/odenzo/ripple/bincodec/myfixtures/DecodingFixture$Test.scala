package com.odenzo.ripple.bincodec.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.FunSuite
import spire.math.UByte

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.utils.{ByteUtils, FixtureUtils, RippleBase58}
import com.odenzo.ripple.bincodec.{Decoded, Encoded, EncodedField, EncodedNestedVals, OTestSpec}

/** This test is designed to process Transaction Request and Response files */
class DecodingFixture$Test extends FunSuite with OTestSpec with ByteUtils with FixtureUtils {

  val txnFixt: List[(JsonObject, JsonObject)] = loadTransactions("/mytests/all_txns.json")

  /** See if we can get the correct Signing Public Key and Hash to start */
  def decodeOne(rq: JsonObject, rs: JsonObject): Either[RippleCodecError, List[Decoded]] = {

    
    logger.info(s"Response: ${rs.asJson.spaces4}")

    val result         = findRequiredObject("result", rs)
    val txjson         = findRequiredObject("tx_json", result)
    val txblob: String = findRequiredStringField("tx_blob", result)

    TxBlobBuster.bust(txblob)

  }

  test("ALL") {
    val done =
      txnFixt.zipWithIndex.traverse {
        case ((rq: JsonObject, rs: JsonObject), indx: Int) ⇒
          logger.info(s"\n\n\n====== Executing Case $indx =======")
          decodeOne(rq, rs)
      }

    val ok: List[List[Decoded]] = getOrLog(done)

  }

  def dumpNestedFieldsInfo(nested: EncodedNestedVals): Unit = {
    import com.odenzo.ripple.bincodec.syntax.debugging._
    logger.info(s"The tree: ${nested.show}")
  }

  test("Specific Cases") {
    val done =
      txnFixt.zipWithIndex.drop(10).take(1).traverse {
        case ((rq: JsonObject, rs: JsonObject), indx: Int) ⇒
          logger.info(s"\n\n\n====== Executing Case $indx =======")
          decodeOne(rq, rs)
      }

    val ok: List[List[Decoded]] = getOrLog(done)

  }

}