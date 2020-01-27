package com.odenzo.ripple.bincodec.setup

import scala.collection.immutable

import com.odenzo.ripple.bincodec._
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.literal._
import _root_.scodec.bits._

class DecoderControllerTest extends OTestSpec {

  val rq =
    json"""{
  "Account": "rMBzp8CgpE441cp5PVyA9rpVV7oT8hP3ys",
  "Expiration": 595640108,
  "Fee": "10",
  "Flags": 524288,
  "OfferSequence": 1752791,
  "Sequence": 1752792,
  "SigningPubKey": "03EE83BB432547885C219634A1BC407A9DB0474145D69737D09CCDC63E1DEE7FE3",
  "TakerGets": "15000000000",
  "TakerPays": {
    "currency": "USD",
    "issuer": "rvYAfWj5gh67oV6fW32ZzP3Aw4Eubs59B",
    "value": "7072.8"
  },
  "TransactionType": "OfferCreate",
  "TxnSignature": "30440220143759437C04F7B61F012563AFE90D8DAFC46E86035E1D965A9CED282C97D4CE02204CFD241E86F17E011298FC1A39B63386C74306A5DE047E213B0F29EFA4571C2C",
  "hash": "73734B611DDA23D3F5F62E20A173B78AB8406AC5015094DA53F53D39B9EDB06C"

}"""

  val rs =
    hex"""120007220008000024001ABED82A2380BF2C2019001ABED764D55920AC9391400000000000000000000000000055534400000000000A20B3C85F482532A9578DBB3950B85CA06594D165400000037E11D60068400000000000000A732103EE83BB432547885C219634A1BC407A9DB0474145D69737D09CCDC63E1DEE7FE3744630440220143759437C04F7B61F012563AFE90D8DAFC46E86035E1D965A9CED282C97D4CE02204CFD241E86F17E011298FC1A39B63386C74306A5DE047E213B0F29EFA4571C2C8114DD76483FACDEE26E60D8A586BB58D09F27045C46"""

  // scribe.info(s"Field Reference Informtion:\n ${dd.show}\n\n")
  val expected: ByteVector =
    hex"1200142280000000240000000663D513BDC0B97008000000000000000000000000004E5A440000000000CD91C65EC7AB628A95AB34E45F1DE19C30CFCA53684000000005F5E1007321ED3AE225CA3BFCC3EE33A934A73C52135315F112BE7FB6F566C67D126753DEEA5A74405BDA5BC162AE75AD24F0103D16022800552615603307AEC501CB3F0632A6127E6021A0156C9F11E1148C54F827FD6CC11729FD2041DA6373A32D9276CC70890B81147AD346725481790E2A15CFF0C08676F0411D2494F9EA7D1054657374205472616E73616374696F6E7E18746578742F706C61696E3B636861727365743D5554462D38E1F1"

  val got: ByteVector =
    hex"1200142280000000240000000663D513BDC0B97008000000000000000000000000004E5A440000000000CD91C65EC7AB628A95AB34E45F1DE19C30CFCA53684000000005F5E1007440067A915B95423D7C99A4E926258AEEED403362CC036FDEC0359E3EE004A74D2CA627212C5979EA7E186A82A93FA59F4A6175E01867A163D9E64717BB00B8620981147AD346725481790E2A15CFF0C08676F0411D2494F9EA7D1054657374205472616E73616374696F6E7E18746578742F706C61696E3B636861727365743D5554462D38E1F1"

  val src = expected

  test("Decoding") {
    val foo = Setup.config.fields.length
    DecoderController.decode(src.bits).require
  }
}