package com.odenzo.ripple.bincodec.reference

import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.OTestSpec

class DefinitionDataTest extends FunSuite with OTestSpec {


  test("Constants") {
    DefinitionData.pathSetEnd.toHex shouldEqual "00"
    DefinitionData.pathSetAnother.toHex shouldEqual "FF"
    DefinitionData.objectEndMarker.toHex shouldEqual "E1"
    DefinitionData.arrayEndMarker.toHex shouldEqual "F1"

  }
  
}
