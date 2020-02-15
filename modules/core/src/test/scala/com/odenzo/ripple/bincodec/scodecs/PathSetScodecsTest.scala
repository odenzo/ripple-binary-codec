package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.models.XRPLPathSet
import scodec.bits._
import io.circe.literal._
import scodec.DecodeResult

class PathSetScodecsTest extends OTestSpec with PathSetScodecs {

  private val problemPaths = json"""

                       [
                             [
                               {
                                 "currency" : "CNY",
                                 "issuer" : "rKiCet8SdvWxPXnAgYarFUXMh1zCPz432Y"
                               },
                               {
                                 "currency" : "CNY",
                                 "issuer" : "razqQKzJRdB4UxFPWf5NEpEG3WMkmwgcXA"
                               },
                               {
                                 "currency" : "XRP"
                               },
                               {
                                 "currency" : "XCN",
                                 "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                               }
                             ],
                             [
                               {
                                 "currency" : "CNY",
                                 "issuer" : "razqQKzJRdB4UxFPWf5NEpEG3WMkmwgcXA"
                               },
                               {
                                 "currency" : "CNY",
                                 "issuer" : "rKiCet8SdvWxPXnAgYarFUXMh1zCPz432Y"
                               },
                               {
                                 "currency" : "XRP"
                               },
                               {
                                 "currency" : "XCN",
                                 "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                               }
                             ],
                             [
                               {
                                 "currency" : "USD",
                                "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                               },
                               {
                                 "currency" : "USD",
                                 "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                               },
                               {
                                 "currency" : "XRP"
                               },
                               {
                                 "currency" : "XCN",
                                 "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                               }
                             ],
                             [
                               {
                                 "currency" : "USD",
                                 "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                               },
                               {
                                 "currency" : "USD",
                                 "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                               },
                               {
                                 "currency" : "XRP"
                               },
                               {
                                 "currency" : "XCN",
                                 "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                               }
                             ],
                             [
                               {
                                 "currency" : "EUR",
                                 "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                               },
                               {
                                 "currency" : "USD",
                                 "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                               },
                               {
                                 "currency" : "XRP"
                               },
                               {
                                 "currency" : "XCN",
                                 "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                               }
                             ],
                             [
                               {
                                 "currency" : "USD",
                                 "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                               },
                               {
                                 "currency" : "EUR",
                                 "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                               },
                               {
                                 "currency" : "XRP"
                               },
                               {
                                 "currency" : "XCN",
                                 "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                               }
                             ]
                           ]"""

  val srcJson = json""" {   "Paths": [
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
          ]
          }"""

  private val pathset =
    // 0112 is field id. Need to reverse the resulting PathSet members
    hex"_0112_01_A4AB176547A22ED23E6D8C3138780526830081D2_30_0000000000000000000000004E5A440000000000_1A255086B5137A6E57079B1B4FFF4F75C61B4F7F_00"

  private val singlePath = hex"""
  01_A4AB176547A22ED23E6D8C3138780526830081D2_
  30_0000000000000000000000004E5A440000000000_1A255086B5137A6E57079B1B4FFF4F75C61B4F7F
  _00""".bits

  // 01 says its account only then no VL account
  private val pathstepA = hex"01_A4AB176547A22ED23E6D8C3138780526830081D2".bits
  private val pathstepB = hex"30_0000000000000000000000004E5A440000000000_1A255086B5137A6E57079B1B4FFF4F75C61B4F7F".bits

  test("Single PathSet") {
    val res: DecodeResult[XRPLPathSet] = xrplPathSet.decode(pathset.drop(2).bits).require
    scribe.debug(s"PathSet: ${pprint.apply(res.value)}")
  }

  import PathStepScodecs.xrplPathStep

  test("Step 0x01") {
    val res = xrplPathStep.decode(pathstepA).require
    scribe.debug(s"Result: $res")
  }

  test("Step 0x30") {
    val res = xrplPathStep.decode(pathstepB).require
    scribe.debug(s"Result: $res")
  }

}
