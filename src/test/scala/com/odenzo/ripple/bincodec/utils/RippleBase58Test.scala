package com.odenzo.ripple.bincodec.utils

import org.scalatest.FunSuite

import com.odenzo.ripple.bincodec.OTestSpec

class RippleBase58Test extends OTestSpec with RippleBase58 {


  test("58Check"){
    val secret = "ssntd6Z1AiHQMpr6k32rkchXccWty"

    val d: Array[Byte] = decode(secret)
    val re: String = encode(d)
    re shouldEqual secret

    
  }
}
