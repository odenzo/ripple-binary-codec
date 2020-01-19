package com.odenzo.ripple.bincodec.codecs
import cats._
import cats.data._
import cats.implicits._
import spire.math.UInt
import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.OTestSpec
class UIntCodecsTest extends OTestSpec with TrivialCodecFn {

  test("UInt8 All") {
    (0 to 255).foreach { i =>
      expectSuccess(TrivialCodecFn.encodeUInt8(i)) { bv =>
        import spire.math.UByte
        scribe.debug(s"${bv.toHex} \t= $i   ")
        bv.length shouldEqual 1
        UByte(bv.head) shouldEqual UByte(i)
      }
    }
  }

  test("UInt8") {
    expectSuccess(TrivialCodecFn.encodeUInt8(255)) { bv =>
      bv.toHex shouldEqual "ff"
    }

  }
  test("UInt8 Negative") {
    expectFailure(TrivialCodecFn.encodeUInt8(-1)) {
      case e: BinCodecLibError => scribe.info(s"Got Excepted ${e.show}")
    }

  }

  test("UInt 16") {
    expectSuccess(TrivialCodecFn.encodeUInt16(255)) { bv =>
      bv.toHex shouldEqual "00ff"
    }
  }

  test("UInt 32") {
    expectSuccess(TrivialCodecFn.encodeUInt32(255)) { bv =>
      bv.toHex shouldEqual "000000ff"
    }
  }

  test("UInt 64") {
    expectSuccess(TrivialCodecFn.encodeUInt64(BigInt(255))) { bv =>
      bv.toHex shouldEqual "00000000000000ff"
    }
  }

  test("UInt 64 Under") {
    expectFailure(TrivialCodecFn.encodeUInt64(BigInt(-1))) {
      identity
    }
  }

  test("UInt 64 Over") {
    import spire.math.ULong
    expectFailure(TrivialCodecFn.encodeUInt64(ULong.MaxValue.toBigInt + 1)) {
      identity
    }
  }
  test("UInt 64 Max") {
    import spire.math.ULong
    expectSuccess(TrivialCodecFn.encodeUInt64(ULong.MaxValue)) { bv =>
      bv.length shouldBe 8
      bv.toHex.iterator.forall(_ == 'f') shouldBe true
    }
  }
}
