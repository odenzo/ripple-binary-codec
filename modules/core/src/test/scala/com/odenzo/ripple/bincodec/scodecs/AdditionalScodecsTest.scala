package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.config.RippleConfig
import com.odenzo.ripple.bincodec.setup.Setup
import scodec._
import scodec.bits._
import spire.math.{UInt, UShort}

class AdditionalScodecsTest extends OTestSpec with AdditionalScodecs {

  private val config: RippleConfig = RippleConfig.loadFromDefaultFile() match {
    case Left(err) => fail(s"Couldn't load Default Config: ${err}")
    case Right(v)  => v
  }

  test("All Transaction Type") {
    // Filtering out -1 UNKNOWN for now. As it isn't a UINT
    val toTest = config.transactionTypes.filter(_._2 >= 0).map {
      case (name, code) =>
        val binary = scodec.codecs.uint16.encode(code).require
        TestParam(binary, name, true)
    }
    implicit val logTest: Boolean = false
    toTest.foreach { tp =>
      roundTripScodec(tp, xrplTransactionType)
    }
  }

  test("All LedgerEntry Type") {
    // Filtering out -1 UNKNOWN for now. As it isn't a UINT
    val toTest = config.ledgerEntryTypes.filter(_._2 >= 0).map {
      case (name, code) =>
        val binary = scodec.codecs.uint16.encode(code).require
        TestParam(binary, name, true)
    }
    implicit val logTest: Boolean = true
    toTest.foreach { tp =>
      roundTripScodec(tp, xrplLedgerEntryType)
    }
  }
}
