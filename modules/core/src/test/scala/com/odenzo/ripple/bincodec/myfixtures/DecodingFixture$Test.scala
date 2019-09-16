package com.odenzo.ripple.bincodec.myfixtures

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.testkit.JsonReqRes
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.{Decoded, EncodedSTObject, OTestSpec, BinCodecLibError}

/** This test is designed to process Transaction Request and Response files */
class DecodingFixture$Test extends FunSuite with OTestSpec with ByteUtils {

  val txnFixt: Either[BinCodecLibError, List[JsonReqRes]] = loadRequestResponseFixture("/mytests/SignRqRs.json")

  /** See if we can get the correct Signing Public Key and Hash to start */
  def decodeOne(rr: JsonReqRes): Either[BinCodecLibError, List[Decoded]] = {

    scribe.info(s"Response: ${rr.rs.asJson.spaces4}")
    findTxBlobInReply(rr.rs).flatMap(TxBlobBuster.bust)

  }

  test("ALL") {
    val done = for {
      fixture <- txnFixt
      withIndex = fixture.zipWithIndex // Instead of traverseIndexM so can prune
      decoded <- withIndex.traverse {
        case (rr, indx: Int) =>
          scribe.info(s"\n\n\n====== Executing Case $indx =======")
          decodeOne(rr)
      }
    } yield decoded
    getOrLog(done)

  }

}
