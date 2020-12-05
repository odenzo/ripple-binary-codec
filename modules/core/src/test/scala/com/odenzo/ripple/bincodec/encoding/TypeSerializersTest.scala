package com.odenzo.ripple.bincodec.encoding

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.setup.{DecoderController, EncoderController, Setup}
import io.circe.{Json, JsonObject}
import io.circe.literal._
import scodec.DecodeResult
import scodec.bits._

class TypeSerializersTest extends OTestSpec {

  import scodec.bits.ByteVector

  val rconfig = Setup.config

  val inorder = json"""
                  {
                   "TransactionType": "EscrowCancel"   ,
                  "OfferSequence": 25   ,
                   "Account": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"    ,
                   "Owner": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"
                  }
  """

  val sample =
    json"""
      {
       "Owner": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2",
       "Account": "r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2"    ,
       "TransactionType": "EscrowCancel"   ,
      "OfferSequence": 25
      }
    """

  val bin = hex"""1200042019000000198114EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA8214EE5F7CF61504C7CF7E0C22562EB19CC7ACB0FCBA"""

  test("Decoding") {
    val res: DecodeResult[JsonObject] = DecoderController.decode(bin.bits)
    scribe.info(s"Result: ${res.value}")
  }

  test("Encoding in Order") {
    val res: BitVector = EncoderController.encode(inorder.asObject.get, forSigning = false)
    res.bytes shouldEqual bin
  }

  test("Encoding out of Order") { // Note this isn't testing nested.
    val res: BitVector = EncoderController.encode(sample.asObject.get, forSigning = false)
    res.bytes shouldEqual bin
  }
}
