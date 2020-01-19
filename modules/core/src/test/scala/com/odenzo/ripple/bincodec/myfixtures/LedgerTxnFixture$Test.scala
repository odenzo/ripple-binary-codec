package com.odenzo.ripple.bincodec.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.Decoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax._
import scribe.Level

import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.testkit.TestLoggingConfig
import com.odenzo.ripple.bincodec.testkit.TestRegimes

/** This test is designed to process Transaction Request and Response files, usually signing */
class LedgerTxnFixture$Test extends OTestSpec {

  val files = List("ledger_direct_txn_good_3.json", "ledger_direct_txn_good_2.json", "ledger_direct_txn_good_1.json")

  test("ALL") {
    TestLoggingConfig.setAll(Level.Info)
    files.map { file =>
      logger.info(s"Doing File $file")
      val complete = for {
        lobj <- loadAndPrepare(file)
        done <- lobj.traverseWithIndexM {
          case (txnObj, indx) =>
            logger.info(s"Doing Index: $indx")
            TestRegimes.testLedgerTxn(txnObj)
        }
      } yield done

      getOrLog(complete)
    }
  }

  test("Problems") {
// Test an index, but do the one before too which presumably passed
    TestLoggingConfig.setAll(Level.Debug)
    val file        = files.head
    val indexToTest = 15
    logger.info(s"Doing File $file")
    val complete = for {
      lobj <- loadAndPrepare(file)
      done <- lobj.zipWithIndex.slice(indexToTest - 1, indexToTest + 1).traverse {
        case (txnObj, indx) =>
          logger.info(s"Doing Index: $indx")
          TestRegimes.testLedgerTxn(txnObj)
      }
    } yield done
    getOrLog(complete)
  }

  def loadAndPrepare(file: String): Either[BinCodecLibError, List[Json]] = {
    for {
      json <- loadJsonResource(s"/mytests/$file")
      lobj <- decode(json, Decoder[List[JsonObject]])
      nometa = lobj.map(o => o.remove("metaData").asJson) // metaData not serialized and noisy
    } yield nometa
  }

}
