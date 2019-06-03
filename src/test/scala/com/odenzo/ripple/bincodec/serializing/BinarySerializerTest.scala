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

  test("EncodeVL"){

    val testRange = Range(1, 918744).inclusive
logger.info(s"Testing for Range $testRange")
   val correctAttempt: Either[AppError, List[List[UByte]]] = loadRightAnswers(testRange.start, testRange.end)
        val correct: Vector[List[UByte]] = correctAttempt.right.value.toVector

    logger.info(s"Got Correct Vector for ${correct.size}")

    //ScriptObjectMirror.
   testRange.foreach { len ⇒
      // My Attempt
      val calculated: BinarySerializer.RawEncodedValue = getOrLog(VLEncoding.encodeVL(len))

      val calcBytes: Seq[UByte] = calculated.ubytes
      val ref: List[UByte] = correct(len - testRange.start)
//      logger.info(s"Ref  $len Bytes: " + MyByte.myByteList2Bits(ref))
//      logger.info(s"Calc $len Bytes: " + MyByte.myByteList2Bits(calcBytes))

      (calcBytes == ref) shouldBe true
    }

  }


  def loadRightAnswers(min:Int=1, max:Int=topOfRange3): Either[AppError, List[List[UByte]]] = {
    import javax.script.ScriptEngineManager

    val engine = new ScriptEngineManager().getEngineByName("nashorn")
    logger.info("Javascript: \n " + js)
    val loader: AnyRef = engine.eval(js)
    logger.info("Loader: " + loader)
    //val res: AnyRef = engine.eval(s"fullCall($len)")
    val res = engine.eval(s"allAtOnce($min,$max)")
    val json: ErrorOr[Json] = CirceUtils.parseAsJson(res.toString)
    val parsed: Either[AppError, List[List[Int]]] = json.flatMap { j ⇒
      CirceUtils.decode(j.as[List[List[Int]]], j, "Decoding JS")
    }
    val asMyBytes = parsed.map { lli: List[List[Int]] ⇒
          lli.map { li: List[Int] ⇒
            li.map((v: Int) ⇒ UByte(v))
          }
    }
    asMyBytes
  }

  def javascriptArrayResToMyBytes(res:String): ErrorOr[List[UByte]] = {
    val json: ErrorOr[Json] = CirceUtils.parseAsJson(res)
    val ans: ErrorOr[List[UByte]] = json.flatMap { j: Json ⇒
      val ar: List[Json] = j.asArray.map(_.toList).getOrElse(List.empty)
      val asInts: Result[List[Int]] = ar.traverse(_.as[Int])
      val wrapped: ErrorOr[List[Int]] = AppJsonDecodingError.wrapResult(asInts, j, "Decoding JS")
      val done: Either[AppError, List[UByte]] = wrapped.map(l⇒ l.map(i ⇒ UByte(i)))
      done
    }

    ans
  }

//  test("Reference By Mirror") {
//    import jdk.nashorn.api.scripting.ScriptObjectMirror
//
//    val engine = new ScriptEngineManager().getEngineByName("nashorn")
//    logger.info("Javascript: \n " + js)
//    val loader: AnyRef = engine.eval(js)
//    val res: AnyRef = engine.eval(s"encodeVL(500)")
//    logger.info("Res: " +res)
//    import javax.script.Invocable
//    val invocable = engine.asInstanceOf[Invocable]
//    val result: AnyRef = invocable.invokeFunction("encodeVL", 500)
//    ScriptObjectMirror   // Think we wrap using this on the javascript side.
//    def fun3(mirror: ScriptObjectMirror): Unit = {
//      System.out.println(mirror.getClassName + ": " + util.Arrays.toString(mirror.getOwnKeys(true)))
//    }
//  }


  def printMaxMinInt(label: String, v: Seq[Int]): Unit = {
    val max = v.max
    val min = v.min
    logger.info(s"$label Max $max  Min $min")
  }

  def printMaxMin(label:String, v:Seq[UByte]): Unit = {
    val max = v.max
    val min = v.min
    logger.info(s"$label Max $max  Min $min")
  }


  test("unsigned binary") {
    //


    val byteLens = Seq(1,2,4,8)

    byteLens.map { len ⇒
      val maxUInt: BigInt = BigInt(len)
        Math.pow(2d, len.toLong * 8d) - 1d // FIXME: Integral Power?
     logger.info(s"Bytes: $len max unsigned is: $maxUInt")
    maxUInt
    }


  }


  val js =
    """
      |function encodeVL(len)   {
      |     length = len;
      |     lenBytes = new Uint8Array(4);
      |    if (length <= 192) {
      |        lenBytes[0] = length;
      |        return lenBytes.subarray(0, 1);
      |    } else if (length <= 12480) {
      |        length -= 193;
      |        lenBytes[0] = 193 + (length >>> 8);
      |        lenBytes[1] = length & 0xff;
      |        return lenBytes.subarray(0, 2);
      |    } else if (length <= 918744) {
      |        length -= 12481;
      |        lenBytes[0] = 241 + (length >>> 16);
      |        lenBytes[1] = (length >> 8) & 0xff;
      |        lenBytes[2] = length & 0xff;
      |        return lenBytes.subarray(0, 3);
      |    }
      |    throw new Error('Overflow error');
      |}
      |
      | 
      |
      |function convertToArray(x) {
      |    var a
      |
      |    switch (x.length) {
      |        case 1:
      |            a = [x[0]];
      |            break;
      |        case 2:
      |            a = [x[0], x[1]];
      |            break;
      |        case 3:
      |            a = [x[0], x[1], x[2]]
      |            break;
      |    }
      |     return a;
      |}
      |
      |function fullCall(len) {
      |     var x = encodeVL(len) ;
      |     var a = convertToArray(x) ;
      |     var s = JSON.stringify(a) ;
      |     return s      ;
      |}
      |
      |function allAtOnce(min,max) {
      |  var json = "["
      |  for (i=min; i<=max;i++) {
      |        var elem = fullCall(i)
      |        json += elem
      |        if (i != max) json += ",\n"
      |  }
      |  return json + "]"
      }
    """.stripMargin
}
