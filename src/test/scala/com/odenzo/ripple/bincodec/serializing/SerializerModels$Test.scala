package com.odenzo.ripple.bincodec.serializing

import cats.implicits._
import org.scalatest.{Assertion, FunSuite}
import spire.math.UByte

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.serializing.BinarySerializer.{Encoded, RawEncodedValue}

/**
  * Test the core model classes here to avoid lots of duplication
  *
  */
class SerializerModels$Test extends FunSuite with OTestSpec {

  val someBytes: List[UByte]    = (0.toInt to 255).map(UByte(_)).toList
  val bigVal: RawEncodedValue   = RawEncodedValue(someBytes)
  val emptyVal: RawEncodedValue = RawEncodedValue(List.empty[UByte])

  def testConsistentWithBase[T<:Encoded](sub:T): Assertion = {
    val encoded :Encoded = sub
    sub.encoded shouldEqual encoded.encoded
    sub.rawBytes shouldEqual encoded.rawBytes

  }
  test("RawEncodedValue Length") {

    bigVal.rawBytes.length shouldEqual 256
    emptyVal.rawBytes.length shouldEqual 0
    bigVal.encoded.length shouldEqual 1
    emptyVal.encoded.length shouldEqual 1

    Seq(bigVal,emptyVal).map(testConsistentWithBase)

  }
}
