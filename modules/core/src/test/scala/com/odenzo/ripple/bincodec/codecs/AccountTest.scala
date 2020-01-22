package com.odenzo.ripple.bincodec.codecs

import scodec.bits.Bases.Alphabets
import cats._
import cats.data._
import cats.implicits._
import scodec.bits._
import spire.math.{UByte, UInt}

import com.odenzo.ripple.bincodec.OTestSpec
import io.circe.syntax._
class AccountTest extends OTestSpec with AccountIdCodecs {

  import org.scalacheck.Gen
  val hexGen = Gen.hexStr

  /** This is interesting test because I am not sure the true functionality, pad left with 0 to meet the expected size? */
  test("Simple") {
    // 33-char
    val account = "rmPD5tJXdk3h4guoCsNADeDXRzmjvG3Ez"
    // 40864D99FE19C6B19B0B7BA865B9A4A552173A896_8314B5F
    expectSuccess(encodeAccountNoVL(account)) { bv =>
      scribe.debug(s"Hex: ${bv.toHex(Alphabets.HexUppercase)}")
      bv.length shouldEqual 20

    }
  }
  test("SimpleVL") {
    val account = "rmPD5tJXdk3h4guoCsNADeDXRzmjvG3Ez"
    expectSuccess(encodeAccountNoVL(account)) { bv =>
      scribe.debug(s"Hex: ${bv.toHex(Alphabets.HexUppercase)}")
      val ubl = bv.toArray.map(b => UByte(b))
      scribe.debug(s"UBytes: \n " + ubl.mkString("\n"))
      bv.length shouldEqual 20

    }
  }

  test("Web Example") {
    val account = "rJrRMgiRgrU6hDF4pgu5DXQdWyPbY35ErN"
    expectSuccess(encodeAccount(account)) { bv =>
      scribe.debug(s"Hex: ${bv.toHex(Alphabets.HexUppercase)}")

      bv.length shouldEqual 21 // 1 for VL cause we now its 20 or so.

    }
  }
}
