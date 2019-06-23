package com.odenzo.ripple.bincodec

import java.net.URL
import scala.io.{BufferedSource, Source}

import cats.implicits._
import io.circe.Json
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, RippleCodecError}
import cats._
import cats.data._
import cats.implicits._

trait OTestUtils  {


  /**
    * This will load from resources/test/fixtures/...
    * Most of those were stolen from Ripple Javascript.
    *
    * @param in  JSON File Name as input to a test fixture
    * @param out JSON File Name matching the desired result
    */
  def loadFixture(in: String, out: String): ErrorOr[(Json, Json)] = {

    for {
      inJson <- loadJsonResource(s"/test/fixtures/$in")
      okJson ← loadJsonResource(s"/test/fixtures/$out")
    } yield (inJson, okJson)

  }

  def loadJsonResource(path: String): Either[RippleCodecError, Json] = {
    BinCodecExeption.wrap(s"Getting Resource $path"){
      val resource: URL = getClass.getResource(path)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }




  /**
    *
    * @param got  Value of field, without field marker
    * @param expected Expected value of fields without field marker
    * @return
    */
  def analyzeAmount(got: String, expected: String) = {

    val isXRP: Either[RippleCodecError, Boolean] = ByteUtils
                                                   .hex2ubyte("0" + got.drop(2).head)
                                                   .map(b ⇒ (b | UByte(8)) === UByte(0))

    isXRP.map {
      case true => scribe.info("XRP Value Deal With IT")
      case false ⇒
        scribe.info("Analyzing Suspected Fiat Amount")
        val gotFields: Either[RippleCodecError, (List[UByte], List[UByte], List[UByte])]      = breakFiat(got)
        val expectedFields: Either[RippleCodecError, (List[UByte], List[UByte], List[UByte])] = breakFiat(expected)
    }

  }

  /** Breaks down to UBytes for the amount, currency amd issuer */
  def breakFiat(hex: String): Either[RippleCodecError, (List[UByte], List[UByte], List[UByte])] = {

    val all: Either[RippleCodecError, List[UByte]] = ByteUtils.hex2ubytes(hex)
    val amount                             = all.map(_.take(8)) // Top 64 is amount in sign and flag
    val currency                           = all.map(_.slice(8, 28)) // 160 bits
    val issuer                             = all.map(_.slice(32, 52)) // another 160 bits
    (amount, currency, issuer).mapN((_, _, _))
  }

  /** Get Top 2 bits, Exponent (Shifted) anf the mantissa in that order in a list.
    * Moved down so the 2 bits in ULong has value 3 is both set etc.
    * */
  def breakFiatAmount(fields: ULong): List[ULong] = {

    // We char about the first 10 bits contains in the first two bytes

    val topMask: ULong      = ULong(0xC000000000000000L)
    val expMask: ULong      = ULong(0xFF) << 54
    val mantissaMask: ULong = ULong(0x3FFFFFFFFFFFFFL) // 13 nibbles

    //    scribe.debug("Masks:\n" + ByteUtils.uLong2Base2Str(topMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(expMask)+
    //    "\n" + ByteUtils.uLong2Base2Str(mantissaMask))

    val top2     = (fields & topMask) >> 62
    val exp      = (fields & expMask) >> 54
    val mantissa = fields & mantissaMask

    List(top2, exp, mantissa)
  }

}
