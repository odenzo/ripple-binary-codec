package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import scodec.bits._
import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.OTestSpec

import org.scalatest._
//
//class HexCodecsTest extends OTestSpec with VLEncodingOldBroken {
//
//  import org.scalacheck.Arbitrary
//  import org.scalacheck.Gen
//  val hexGen = Gen.hexStr
//
//  /** This is interesting test because I am not sure the true functionality, pad left with 0 to meet the expected size? */
//  test("To Small Hexes for All") {
//    val small = "FF"
//    expectFailure(encodeHash256(small)) { identity }
//    expectFailure(encodeHash160(small)) { identity }
//    expectFailure(encodeVector256(List(small, small)))
//  }
//
//  test("To Big Hexes for All") {
//    val big = "FF".repeat(33)
//    expectFailure(encodeHash256(big)) { identity }
//    expectFailure(encodeHash160(big)) { identity }
//    expectFailure(encodeVector256(List(big, big)))
//  }
//
//  test("Max Hash") {
//    val big = "ff".repeat(32)
//    expectSuccess(encodeHash256(big)) { bv =>
//      bv.length shouldBe 32
//      bv.toHex.forall(_ == 'f')
//    }
//  }
//
//  test("Some Property Based Tests") {
//    val big = "ff".repeat(10).padTo(64, '0').reverse // rightPad
//    expectSuccess(encodeHash256(big)) { bv =>
//      bv.length shouldBe 32
//      bv.toHex.forall(c => c == 'f' || c == '0')
//    }
//  }
//
//  test("Some Property Based Tests ON the General Hex") {
//    import scodec.bits.ByteVector
//    // @todo Gen ByteVectors
//    val big   = "ff".repeat(10).padTo(64, '0').reverse // rightPad
//    val bvGen = hex"ff00ff"
//
//    expectSuccess(encodeHex(bvGen.toHex)) { bv =>
//      bvGen shouldEqual bv
//    }
//  }
//
//  test("UInt 64 Max") {
//    import spire.math.ULong
//    expectSuccess(VLEncodingOldBroken.encodeUInt64(ULong.MaxValue)) { bv =>
//      bv.length shouldBe 8
//      bv.toHex.iterator.forall(_ == 'f') shouldBe true
//    }
//  }
//}
