package com.odenzo.ripple.bincodec.testkit

import java.security.MessageDigest

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import io.circe.JsonObject
import io.circe.optics.JsonPath
import scribe.Logging

import com.odenzo.ripple.bincodec.{EncodedSTObject, BinCodecLibError, RippleCodecAPI, BCJsonErr}
import io.circe.optics.JsonPath._

import com.odenzo.ripple.bincodec.reference.HashPrefix
import com.odenzo.ripple.bincodec.utils.ByteUtils

trait TestRegimes extends RippleTestUtils with Logging with CodecTestCreators {

  /** Start to make a standard detailed one as a utility. Exercise all top level functionality
    * even though a bit redundant */
  def testSignRqRs(rr: JsonReqRes): Either[Throwable, Unit] = {
    logger.debug(s"Testing A Signed Request Response:\n ${rr.show}")

    for {
      txjson <- findTxJsonInReply(rr.rs)
      _      <- checkTxBlob(rr.rs)
      _      <- checkHash(txjson)
      _      <- checkSigningSerializationLamely(rr.rs)
    } yield ()
  }

  /** From  */
  def testLedgerTxn(txn: JsonObject): Either[BCJsonErr, JsonObject] = {
    logger.debug(s"Testing A Single Ledger Txn\n ${txn.asJson.spaces4}")
    // No TxBlob but has a hash
    val done = for {
      hash <- checkHash(txn)
    } yield hash
    done.leftMap(e => BCJsonErr("LedgerTxnRegime Failed", txn.asJson, e.some))
  }

}

object TestRegimes extends TestRegimes
