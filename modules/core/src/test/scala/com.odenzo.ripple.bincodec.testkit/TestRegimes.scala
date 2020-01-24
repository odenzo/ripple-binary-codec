package com.odenzo.ripple.bincodec.testkit

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.Json
import scribe.Logging

import com.odenzo.ripple.bincodec.BCJsonErr
//
//trait TestRegimes extends RippleTestUtils with Logging with CodecTestCreators {
//
//  /** Start to make a standard detailed one as a utility. Exercise all top level functionality
//    * even though a bit redundant */
//  def testSignRqRs(rr: JsonReqRes): Either[Throwable, Unit] = {
//    logger.debug(s"Testing A Signed Request Response:\n ${rr.show}")
//
//    for {
//      txjson <- findTxJsonInReply(rr.rs)
//      _      <- checkTxBlob(rr.rs)
//      _      <- checkHash(txjson.asJson)
//    } yield ()
//  }
//
//  /** From  */
//  def testLedgerTxn(txn: Json): Either[BCJsonErr, Json] = {
//    logger.debug(s"Testing A Single Ledger Txn\n ${txn.spaces4}")
//    // No TxBlob but has a hash
//    val done = for {
//      hash <- checkHash(txn)
//    } yield hash
//    done.leftMap(e => BCJsonErr("LedgerTxnRegime Failed", txn, e.some))
//  }
//
//}
//
//object TestRegimes extends TestRegimes
