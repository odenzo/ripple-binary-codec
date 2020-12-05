package com.odenzo.ripple.bincodec.encoding

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.setup.{DecoderController, EncoderController, Setup}
import io.circe.JsonObject
import io.circe.literal._
import scodec.DecodeResult
import scodec.bits._
import io.circe.syntax._

class FieldSortingTest extends OTestSpec {

  /** Correct ordering, or matches the binary at least. */
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

  test("Sort JSON Stable InOrder") {

    val resort: Option[JsonObject] = inorder.asObject.map(EncoderController.preprocessJson(false, _))
    val back: JsonObject           = resort.value
    scribe.info(s"In: ${inorder.spaces4}")
    scribe.info(s"Back: ${back.asJson.spaces4}")
    back.asJson shouldEqual inorder
  }

  test("Sort JSON ") {

    val resort: Option[JsonObject] = sample.asObject.map(EncoderController.preprocessJson(false, _))
    val back: JsonObject           = resort.value
    scribe.info(s"In Bad: ${sample.spaces4}")
    scribe.info(s"Back: ${back.asJson.spaces4}")
    back.asJson shouldEqual sample
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
