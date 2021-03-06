package com.odenzo.ripple.bincodec.myfixtures

import cats._
import cats.data._
import cats.implicits._
import org.scalatest.FunSuite
import scribe.Level

import com.odenzo.ripple.bincodec.testkit.{TestLoggingConfig, JsonReqRes, TestRegimes}
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.{EncodedSTObject, OTestSpec, BinCodecLibError}

/** This test is designed to process Transaction Request and Response files */
class ProblemsFixture$Test extends FunSuite with OTestSpec with ByteUtils {

  private val allTxn = loadRequestResponseFixture("/mytests/SignRqRs_Problems.json")

  test("ALL") {
    TestLoggingConfig.setAll(Level.Info)
    val txns = getOrLog(allTxn)
    val done = txns.traverseWithIndexM {
      case (d, i) =>
        logger.warn(s"Doing Index $i")
        TestRegimes.testSignRqRs(d)
    }
    getOrLog(done)
  }

  test("Specific Cases") {
    TestLoggingConfig.setAll(Level.Debug)
    val txns = getOrLog(allTxn)
    val complete = txns
      .drop(0)
      .take(1)
      .traverse(TestRegimes.testSignRqRs)

    getOrLog(complete)
  }

}
