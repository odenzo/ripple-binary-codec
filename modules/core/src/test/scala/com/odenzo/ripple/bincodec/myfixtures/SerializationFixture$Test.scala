package com.odenzo.ripple.bincodec.myfixtures
//
//import cats._
//import cats.data._
//import cats.implicits._
//
//import com.odenzo.ripple.bincodec.OTestSpec
//import com.odenzo.ripple.bincodec.testkit.TestLoggingConfig
//import com.odenzo.ripple.bincodec.testkit.TestRegimes
//
///** This test is designed to process Transaction Request and Response files, usually signing */
//class SerializationFixture$Test extends OTestSpec {
//
//  private val allTxn = loadRequestResponseFixture("/mytests/SignRqRs.json")
//
//  test("ALL") {
//    TestLoggingConfig.debugLevel()
//    val done = getOrLog(allTxn).traverse(TestRegimes.testSignRqRs)
//    val ok   = getOrLog(done)
//  }
//
//  test("Specific Cases") {
//    getOrLog(allTxn)
//      .drop(1)
//      .take(1)
//      .traverse(v => TestRegimes.testSignRqRs(v))
//  }
//
//}
