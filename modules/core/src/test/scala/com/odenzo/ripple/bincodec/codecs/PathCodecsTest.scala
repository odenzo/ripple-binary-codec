package com.odenzo.ripple.bincodec.codecs

import com.odenzo.ripple.bincodec.OTestSpec

class PathCodecsTest extends OTestSpec {

  val problemPaths = """
                       |
                       |[
                       |      [
                       |        {
                       |          "currency" : "CNY",
                       |          "issuer" : "rKiCet8SdvWxPXnAgYarFUXMh1zCPz432Y"
                       |        },
                       |        {
                       |          "currency" : "CNY",
                       |          "issuer" : "razqQKzJRdB4UxFPWf5NEpEG3WMkmwgcXA"
                       |        },
                       |        {
                       |          "currency" : "XRP"
                       |        },
                       |        {
                       |          "currency" : "XCN",
                       |          "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                       |        }
                       |      ],
                       |      [
                       |        {
                       |          "currency" : "CNY",
                       |          "issuer" : "razqQKzJRdB4UxFPWf5NEpEG3WMkmwgcXA"
                       |        },
                       |        {
                       |          "currency" : "CNY",
                       |          "issuer" : "rKiCet8SdvWxPXnAgYarFUXMh1zCPz432Y"
                       |        },
                       |        {
                       |          "currency" : "XRP"
                       |        },
                       |        {
                       |          "currency" : "XCN",
                       |          "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                       |        }
                       |      ],
                       |      [
                       |        {
                       |          "currency" : "USD",
                       |          "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                       |        },
                       |        {
                       |          "currency" : "USD",
                       |          "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                       |        },
                       |        {
                       |          "currency" : "XRP"
                       |        },
                       |        {
                       |          "currency" : "XCN",
                       |          "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                       |        }
                       |      ],
                       |      [
                       |        {
                       |          "currency" : "USD",
                       |          "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                       |        },
                       |        {
                       |          "currency" : "USD",
                       |          "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                       |        },
                       |        {
                       |          "currency" : "XRP"
                       |        },
                       |        {
                       |          "currency" : "XCN",
                       |          "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                       |        }
                       |      ],
                       |      [
                       |        {
                       |          "currency" : "EUR",
                       |          "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                       |        },
                       |        {
                       |          "currency" : "USD",
                       |          "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                       |        },
                       |        {
                       |          "currency" : "XRP"
                       |        },
                       |        {
                       |          "currency" : "XCN",
                       |          "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                       |        }
                       |      ],
                       |      [
                       |        {
                       |          "currency" : "USD",
                       |          "issuer" : "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B"
                       |        },
                       |        {
                       |          "currency" : "EUR",
                       |          "issuer" : "rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq"
                       |        },
                       |        {
                       |          "currency" : "XRP"
                       |        },
                       |        {
                       |          "currency" : "XCN",
                       |          "issuer" : "r8HgVGenRTAiNSM5iqt9PX2D2EczFZhZr"
                       |        }
                       |      ]
                       |    ]""".stripMargin

  test("Paths with Issuer Only") {
    //todo
  }
}
