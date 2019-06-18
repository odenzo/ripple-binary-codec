package com.odenzo.ripple.bincodec.decoding

import scala.collection.immutable

import org.scalatest.FunSuite
import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.{Decoded, OTestSpec}
import com.odenzo.ripple.bincodec.reference.Definitions
import com.odenzo.ripple.bincodec.encoding.BinarySerializer
import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError

class TxBlobBusterTest extends FunSuite with OTestSpec {

  import com.odenzo.ripple.bincodec.syntax.debugging._
  test("Sample Blob") {

    val dd = Definitions.fieldData
   // logger.info(s"Field Reference Informtion:\n ${dd.show}\n\n")
    val expected: String =
      "1200142280000000240000000663D513BDC0B97008000000000000000000000000004E5A440000000000CD91C65EC7AB628A95AB34E45F1DE19C30CFCA53684000000005F5E1007321ED3AE225CA3BFCC3EE33A934A73C52135315F112BE7FB6F566C67D126753DEEA5A74405BDA5BC162AE75AD24F0103D16022800552615603307AEC501CB3F0632A6127E6021A0156C9F11E1148C54F827FD6CC11729FD2041DA6373A32D9276CC70890B81147AD346725481790E2A15CFF0C08676F0411D2494F9EA7D1054657374205472616E73616374696F6E7E18746578742F706C61696E3B636861727365743D5554462D38E1F1"

    val got =
      "1200142280000000240000000663D513BDC0B97008000000000000000000000000004E5A440000000000CD91C65EC7AB628A95AB34E45F1DE19C30CFCA53684000000005F5E1007440067A915B95423D7C99A4E926258AEEED403362CC036FDEC0359E3EE004A74D2CA627212C5979EA7E186A82A93FA59F4A6175E01867A163D9E64717BB00B8620981147AD346725481790E2A15CFF0C08676F0411D2494F9EA7D1054657374205472616E73616374696F6E7E18746578742F706C61696E3B636861727365743D5554462D38E1F1"

    val src = expected
   def run(src:String): List[Decoded] = {
    logger.info(s"SRC: $src")
    val res: Either[RippleCodecError, List[Decoded]] = TxBlobBuster.bust(src)
    val ok: List[Decoded] = getOrLog(res)
    logger.info(s"SRC: $src")
    logger.info(s"Raw List FieldDecoded $ok")
                                ok

   }

    val expRs: List[Decoded] = run(expected)
    val gotRs: immutable.Seq[Decoded] = run(got)
    logger.info(s"Decoded EXPECTED:\n${expRs.mkString("\n")}")

    val txt: immutable.Seq[String] = gotRs.map(_.show)

    logger.info(s"Decoded GOT:\n${gotRs.mkString("\n")}")
    logger.info(s"Exp: \n${expRs.show.mkString}")
    logger.info(s"Got: \n${gotRs.map(_.show).mkString("Fields:\n", "\n\t", "\n")}")
  }

}
