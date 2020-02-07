package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import scodec.bits._

class PathSetScodecsTest extends OTestSpec with PathSetScodecs {

  val srcJson = """    "Paths": [
            [
              {
                "account": "rGrg8a65omKD6F4moDXRiS82XMXtpM996Z",
                "type": 1,
                "type_hex": "0000000000000001"
              },
              {
                "currency": "NZD",
                "issuer": "rsPEGecnKhQ3bkH3co1uRSD3tUwHDrDz1T",
                "type": 48,
                "type_hex": "0000000000000030"
              }
            ]
          ]"""

  val pathset =
    hex"_0112_01_A4AB176547A22ED23E6D8C3138780526830081D2_30_0000000000000000000000004E5A440000000000_1A255086B5137A6E57079B1B4FFF4F75C61B4F7F_00"

  val singlePath = hex"""
  01_A4AB176547A22ED23E6D8C3138780526830081D2_30_0000000000000000000000004E5A440000000000_1A255086B5137A6E57079B1B4FFF4F75C61B4F7F_00""".bits

  val pathstepA = hex"01_A4AB176547A22ED23E6D8C3138780526830081D2".bits
  val pathstepB = hex"30_0000000000000000000000004E5A440000000000_1A255086B5137A6E57079B1B4FFF4F75C61B4F7F".bits

  test("Single PathSet") {
    xrppathset.decode(pathset.bits).require
  }

  test("Step 0x01") {
    val res = xrplPathStep.decode(pathstepA).require
    scribe.debug(s"Result: $res")
  }

  test("Step 0x30") {
    val res = xrplPathStep.decode(pathstepB).require
    scribe.debug(s"Result: $res")
  }

  test("Single Path") {
    val res = xrppath.decode(singlePath).require
    scribe.debug(s"Result: $res")
  }
}
