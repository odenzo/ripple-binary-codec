package com.odenzo.ripple.bincodec.myfixtures

import cats._
import cats.data._
import cats.implicits._
import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.testkit.{TestLoggingConfig, JsonReqRes, TestRegimes}
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.{EncodedSTObject, OTestSpec, BinCodecLibError}

/** This test is designed to process Transaction Request and Response files */
class ProblemsFixture$Test extends FunSuite with OTestSpec with ByteUtils {

  private val allTxn = loadRequestResponseFixture("/mytests/SignRqRs_Problems.json")

  test("ALL") {
    TestLoggingConfig.debugLevel()
    val txns = getOrLog(allTxn)
    val done = txns.traverse(TestRegimes.testSignRqRs)
    getOrLog(done)
  }

  test("Specific Cases") {
    val txns = getOrLog(allTxn)
    txns
      .drop(1)
      .take(1)
      .traverse(TestRegimes.testSignRqRs)
  }

}
