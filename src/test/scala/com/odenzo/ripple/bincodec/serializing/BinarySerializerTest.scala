package com.odenzo.ripple.bincodec.serializing

import cats.implicits._
import io.circe.Decoder.Result
import io.circe.Json
import org.scalatest.FunSuite
import spire.math.UByte

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.utils.CirceUtils
import com.odenzo.ripple.bincodec.utils.caterrors.CatsTransformers.ErrorOr
import com.odenzo.ripple.bincodec.utils.caterrors.{AppError, AppJsonDecodingError}

/**
  * Me mucking around alot to test Javasceipt and Scala/Java together.
  *
  */
class BinarySerializerTest extends FunSuite with OTestSpec {

  val topOfRange1: Int = 192
  val topOfRange2: Int = 12480
  val topOfRange3: Int = 918744

  test("Out of Range EncodeVL") {
    VLEncoding.encodeVL(0).isLeft shouldBe true
    VLEncoding.encodeVL(918755).isLeft shouldBe true
    VLEncoding.encodeVL(-10).isLeft shouldBe true
  }


  def javascriptArrayResToMyBytes(res: String): ErrorOr[List[UByte]] = {
    val json: ErrorOr[Json] = CirceUtils.parseAsJson(res)
    val ans: ErrorOr[List[UByte]] = json.flatMap { j: Json ⇒
      val ar: List[Json]                      = j.asArray.map(_.toList).getOrElse(List.empty)
      val asInts: Result[List[Int]]           = ar.traverse(_.as[Int])
      val wrapped: ErrorOr[List[Int]]         = AppJsonDecodingError.wrapResult(asInts, j, "Decoding JS")
      val done: Either[AppError, List[UByte]] = wrapped.map(l ⇒ l.map(i ⇒ UByte(i)))
      done
    }

    ans
  }

  def printMaxMinInt(label: String, v: Seq[Int]): Unit = {
    val max = v.max
    val min = v.min
    logger.info(s"$label Max $max  Min $min")
  }

  test("unsigned binary") {
    //

    val byteLens = Seq(1, 2, 4, 8)

    byteLens.map { len ⇒
      val maxUInt: BigInt = BigInt(len)
      Math.pow(2d, len.toLong * 8d) - 1d // FIXME: Integral Power?
      logger.info(s"Bytes: $len max unsigned is: $maxUInt")
      maxUInt
    }

  }

  def printMaxMin(label: String, v: Seq[UByte]): Unit = {
    val max = v.max
    val min = v.min
    logger.info(s"$label Max $max  Min $min")
  }
}
