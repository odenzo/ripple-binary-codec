package com.odenzo.ripple.bincodec.codecs

import com.odenzo.ripple.bincodec.OTestSpec
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.literal._
class UIntCodecsTest extends OTestSpec with ScodecStyleCodecs {

  test("UInt 64") {
    val big = BigInt("1024")
    scodecUInt64(big) match {
      case Right(v) => scribe.info(s"Result ${v.toHex}")
      case Left(e)  => scribe.error(e)
    }

  }

  test("Scodec") {
    val res = scodecUInt8(200)
    scribe.debug(s"Res; $res")
  }
  test("UInt 16") {

    scodecUInt16(255) match {
      case Right(v) => scribe.info(s"Result ${v.toHex}")
      case Left(e)  => scribe.error(e)
    }

  }
}
