package com.odenzo.ripple.bincodec.utils

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import org.scalacheck.Gen
import org.scalacheck.Prop

import com.odenzo.ripple.bincodec.codecs.AccountIdCodecs
import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.bincodec.OTestSpec

//noinspection CorrespondsUnsorted
class RippleBase58Test extends OTestSpec {

  val byteGen: Gen[Byte]  = Gen.chooseNum[Byte](Byte.MinValue, Byte.MaxValue, 0: Byte)
  val alphaGen: Gen[Char] = Gen.oneOf(RippleBase58.alphabet.toSeq)

  // encode => decode

  val byteIso = Prop.forAll(byteGen) { b: Byte =>
    val in: Seq[Byte]                            = Seq(b)
    val out: String                              = RippleBase58.encode(in)
    val iso: Either[BinCodecLibError, Seq[Byte]] = RippleBase58.decode2bytes(out)
    iso match {
      case Right(v: Seq[Byte]) =>
        logger.info(s"IN ${in.toList} => $out => ${v.toList}")
        in.sameElements(v) == true
      case Left(err) => fail(err)
    }
  }

  val strIso = Prop.forAll(alphaGen) { b58: Char =>
    val in: String                               = b58.toString()
    val out: Either[BinCodecLibError, Seq[Byte]] = RippleBase58.decode2bytes(in)
    out match {
      case Left(err) => fail(err)
      case Right(v) =>
        val iso = RippleBase58.encode(v)
        in.sameElements(iso)
    }
  }

  test("58Check") {
    val secret                                       = "ssntd6Z1AiHQMpr6k32rkchXccWty"
    val (s2: String, decode: Seq[Byte], iso: String) = getOrLog(B582Decoded2Encoded(secret))
    s2 shouldEqual iso
  }

  test("Zero Prefix B58") {
    val data          = "rrrrS"
    val ub: Seq[Byte] = getOrLog(RippleBase58.decode2bytes(data))
    logger.debug(s"${data} $ub")
    ub.length shouldEqual 5 // Well, I guess so...
  }

  test("Zero Prefix Bytes") {
    val data = Seq[Byte](0, 0, 23, 23)
    val b58  = RippleBase58.encode(data)
    logger.debug(s"${data} $b58")

  }

  def B582Decoded2Encoded(b58: String): Either[Throwable, (String, Seq[Byte], String)] = {
    for {
      decoded <- RippleBase58.decode2bytes(b58).map(_.toSeq)
      reencoded = RippleBase58.encode(decoded)
      _         = logger.debug(s"$b58 => $decoded => $reencoded")
    } yield (b58, decoded, reencoded)
  }

  test("Some Accounts") {
    List(
      "rmPD5tJXdk3h4guoCsNADeDXRzmjvG3Ez",
      "rL8igKxCefdw8Jnmp2W4wpmgKjTfu1seKA",
      "r9tYoofcDdYrpk3FhYPatvySgjYfkKeUJs"
    ).map { s =>
      import scodec.bits.ByteVector
      val j                  = Json.fromString(s)
      val decoded            = ByteUtils.bytes2hex(getOrLog(RippleBase58.decode2bytes(s)))
      val txblob: ByteVector = getOrLog(AccountIdCodecs.encodeAccount(j))
      logger.info(s"$j\n $decoded \n ${txblob.toHex}")
    }

  }
}
