package com.odenzo.ripple.bincodec.codecs

import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.{OTestSpec, RawValue}
import com.odenzo.ripple.bincodec.utils.caterrors.OErrorRipple

class VLEncodingTest extends OTestSpec {

  test("Empty String") {
    val zero = VLEncoding.encodeVL(0)
    logger.info(s"Zero Len Encoded: $zero")
    zero.isLeft shouldBe false

  }
}
