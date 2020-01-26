package com.odenzo.ripple.bincodec.reference

import scala.util.Try

import io.circe

import com.odenzo.ripple.bincodec.OTestSpec

class RippleConfigTest extends OTestSpec {

  test("Load Default Resource") {
    RippleConfig.loadFromDefaultFile().map { config =>
      scribe.info(s"Res ${smartprint(config)}")
    }

  }
}
