package com.odenzo.ripple.bincodec.setup

import com.odenzo.ripple.bincodec.OTestSpec

class SetupTest extends OTestSpec {

  test("Show Types") {
    val config = Setup.config
    scribe.info(s"Config Loaded")
    val ll = Setup.bindFieldIdToFields()
    scribe.info(s"Types: ${pprint.apply(ll)}")
  }
}
