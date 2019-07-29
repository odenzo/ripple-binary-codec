package com.odenzo.ripple.bincodec.encoding

import io.circe.JsonObject
import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.{EncodedSTObject, OTestSpec, RippleCodecAPI, TestLoggingConfig}

/** This is a quick test to make sure we have the binary encoding correct for multi-signing messages
  * Signers is serialized but not for signing, that checks out ok.
  * SigningPubKey="" is serialized and Signing, so those are the new fields taken care of.
  * But the TxnSignature is still off (when using ED25119 keys. )
  */
class SignFor$Test extends OTestSpec {

  private val finalResponse =
    """
      | {
      |    "id" : "4fbc685a-ce06-48a4-84c4-b50caebee456",
      |    "result" : {
      |        "deprecated" : "This command has been deprecated and will be removed in a future version of the server. Please migrate to a standalone signing tool.",
      |        "tx_blob" : "12000022800000002400000002614000000000989680684000000002FAF0807300811443337FB05F5F844450F07F104970E6AE0F9629AD8314B5F762798A53D543A014CAF8B297CFF8F2F937E8F3E0107321ED1C2BDBACFC2AA2BC761504A16BCD471B7679EAF4F44920ABAF39EC4A19FDB3F774407265824A30ED217E096AAF371433F6064A630265C68858FD95B16A0F4F8CC4068EBE49F8DB9BB554FDF5F39E1516F45E5D7E770F85A52EDC1CDAAB95E3C35709811460C668979F5DAA60AB82F7E3676B04525E58E382E1E0107321ED46ECCE1D2E6CD69C097A00A7C39720A33DA68265B2CAE7C2E72F8FE48E6F60BE74406546B0A507610A9A81C24825A41E25A695C0822E2512B87949B56B6789110A196ACBF31552EBA968A206FD3759DEA52C27C739D4CD8B5ABDD34FAE1414B8F508811463517F16AFA529D53B7F49727CF9F6AFEE02E90AE1E0107321ED4037595AD7D62E82B84AA42AEAE4BDE4018F95641E7DC0E23586562861C3B84474400B8581A006DB587DDE83C8CCBD57EAA0EA9DE42F7E0D3ADA25F3FF18409ED42E32C4FFB4218E6FD081E7DED8CBBDF91814435547A65AD7C235C3B4D8ACD2E704811476D3B92097165E708D37986E9D11E516B30B181FE1E0107321ED73469ADF14F17A6882A111C4D0A2E625114CE6776481F3346332C22BB468F0AB74400A40D17A019F73B235980E0058D3D9ECD485D1EF1F27EA0DD081C8FBEBAE8C8F41CD172E59A74C5CACC2DA02232AF77A73E453F48F4D690A82EB2A103C057B0281148E98089DB1D5EBEE30D8F58B49CB6F5E2848EECAE1F1",
      |        "tx_json" : {
      |            "Account" : "rf3KysxP6McY23JXoNqrSCHXSrLtoNq2fs",
      |            "Amount" : "10000000",
      |            "Destination" : "rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh",
      |            "Fee" : "50000000",
      |            "Flags" : 2147483648,
      |            "Sequence" : 2,
      |            "Signers" : [
      |                {
      |                    "Signer" : {
      |                        "Account" : "r9F6Zntu8KPNxru8D3AnvwZp756oTzhfcZ",
      |                        "SigningPubKey" : "ED1C2BDBACFC2AA2BC761504A16BCD471B7679EAF4F44920ABAF39EC4A19FDB3F7",
      |                        "TxnSignature" : "7265824A30ED217E096AAF371433F6064A630265C68858FD95B16A0F4F8CC4068EBE49F8DB9BB554FDF5F39E1516F45E5D7E770F85A52EDC1CDAAB95E3C35709"
      |                    }
      |                },
      |                {
      |                    "Signer" : {
      |                        "Account" : "rwh9XTixXtqYdg6Fwek2SK9ZPmPLjQT6XQ",
      |                        "SigningPubKey" : "ED46ECCE1D2E6CD69C097A00A7C39720A33DA68265B2CAE7C2E72F8FE48E6F60BE",
      |                        "TxnSignature" : "6546B0A507610A9A81C24825A41E25A695C0822E2512B87949B56B6789110A196ACBF31552EBA968A206FD3759DEA52C27C739D4CD8B5ABDD34FAE1414B8F508"
      |                    }
      |                },
      |                {
      |                    "Signer" : {
      |                        "Account" : "rBqJ4RH9qstrBLpbKZbTsUKuRZ6hxeUieu",
      |                        "SigningPubKey" : "ED4037595AD7D62E82B84AA42AEAE4BDE4018F95641E7DC0E23586562861C3B844",
      |                        "TxnSignature" : "0B8581A006DB587DDE83C8CCBD57EAA0EA9DE42F7E0D3ADA25F3FF18409ED42E32C4FFB4218E6FD081E7DED8CBBDF91814435547A65AD7C235C3B4D8ACD2E704"
      |                    }
      |                },
      |                {
      |                    "Signer" : {
      |                        "Account" : "rDzyn5yrGfoxwqG5cT7fxMriSZN9ZE4jhv",
      |                        "SigningPubKey" : "ED73469ADF14F17A6882A111C4D0A2E625114CE6776481F3346332C22BB468F0AB",
      |                        "TxnSignature" : "0A40D17A019F73B235980E0058D3D9ECD485D1EF1F27EA0DD081C8FBEBAE8C8F41CD172E59A74C5CACC2DA02232AF77A73E453F48F4D690A82EB2A103C057B02"
      |                    }
      |                }
      |            ],
      |            "SigningPubKey" : "",
      |            "TransactionType" : "Payment",
      |            "hash" : "F62B236E38EA9F4FFCB1129608EC93DD10E1AB94A72AE7CB7C21A20CE79D0A82"
      |        }
      |    },
      |    "status" : "success",
      |    "type" : "response"
      |}
    """.stripMargin

  // Check the TxBlob and the hash to see how SigningPubKey was actually encoded.
  // SigningPubKey Its VLEncoded for signing and serialization, nth = 3

  /**
    * Makes sure the TxBlob and Hash are correct.
    * This doesn't (cannot) check the signing serialization part is correct. But we can try and
    * do a diff on the signingEncoded vs binarySerialized (latter for TxBlob)
    */
  test("Correct Serialization and Hash") {
    val json = getOrLog(JsonUtils.parseAsJsonObject(finalResponse))

    val txblob: String      = getOrLog(findResultTxBlob(json))
    val tx_json: JsonObject = getOrLog(findTxJson(json))

    logger.info(s"TxBlob: $txblob")

    import com.odenzo.ripple.bincodec.syntax.debugging._

    val signingEncoded: EncodedSTObject = getOrLog(RippleCodecAPI.binarySerializeForSigning(tx_json))
    logger.debug(s"Signing ${signingEncoded.show}")

    val binaryEncoded = getOrLog(RippleCodecAPI.binarySerialize(tx_json))
    logger.debug(s"Binary ${binaryEncoded.show}")
    val hex = binaryEncoded.toHex
    logger.info(s"Lengths ${txblob.length} == ${hex.length}")
    logger.info(s"Expexted and  Bin :\n $txblob \n $hex")
    hex shouldEqual txblob

  }

}
