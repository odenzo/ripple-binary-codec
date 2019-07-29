package com.odenzo.ripple.bincodec.utils

import com.odenzo.ripple.bincodec.OTestSpec

class RippleBase58Test extends OTestSpec with RippleBase58 {

  test("58Check") {
    val secret              = "ssntd6Z1AiHQMpr6k32rkchXccWty"
    val d: IndexedSeq[Byte] = decode(secret).toIndexedSeq
    val re: String          = encode(d.toIndexedSeq)
    re shouldEqual secret

  }
}
