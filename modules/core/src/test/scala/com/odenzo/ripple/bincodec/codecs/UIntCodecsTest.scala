package com.odenzo.ripple.bincodec.codecs

import com.odenzo.ripple.bincodec.OTestSpec
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.literal._
class UIntCodecsTest extends OTestSpec with UIntCodecs {

  test("UInt 64") {
    val big = json""" "1024"  """
    encodeUInt64(big) match {
      case Right(v) => scribe.info(s"Result ${v.toHex}")
      case Left(e)  => scribe.error(e)
    }

  }

  test("Scodec") {
    val res = scodecUInt8(200)
    scribe.debug(s"Res; $res")
  }
  test("UInt 16") {
    val big = json""" 255  """
    encodeUInt16(big) match {
      case Right(v) => scribe.info(s"Result ${v.toHex}")
      case Left(e)  => scribe.error(e)
    }

  }
}
