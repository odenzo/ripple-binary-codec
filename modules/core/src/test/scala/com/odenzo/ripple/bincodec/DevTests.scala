package com.odenzo.ripple.bincodec

import io.circe.Json
import io.circe.literal._

import com.odenzo.ripple.bincodec.testkit.TestRegimes

class DevTests extends OTestSpec {

  test("OfferCancelLEedgerTxn") {
    val json: Json = json""" {
    "Account" : "rQ3fNyLjbvcDaPNS4EAJY8aT9zR3uGk17c",
    "Fee" : "12",
    "Flags" : 0,
    "LastLedgerSequence" : 50047250,
    "OfferSequence" : 11898197,
    "Sequence" : 11898201,
    "SigningPubKey" : "039451ECAC6D4EB75E3C926E7DC7BA7721719A1521502F99EC7EB2FE87CEE9E824",
    "TakerGets" : "7391586793",
    "TakerPays" : {
      "currency" : "CNY",
      "issuer" : "rJ1adrpGS3xsnQMb9Cw54tWJVFPuSdZHK",
      "value" : "14464.89186027806"
    },
    "TransactionType" : "OfferCreate",
    "TxnSignature" : "304402202DEC4F40607E7BAAB9F8DBC814B203D4E8627BAC007CE284E1D3F31476AB93F90220527DEC0D9E980D3630B8E1BD639718E66ECF34F792199C73CFF10D0C1B513A9D",
    "hash" : "F96D5B01F157ACB7CB3594F035C52AE9368398ABFF7DA95A982B818DB1067D30",
    "metaData" : {
      "AffectedNodes" : [
        {
          "ModifiedNode" : {
            "FinalFields" : {
              "Flags" : 0,
              "IndexNext" : "0000000000000000",
              "IndexPrevious" : "0000000000000000",
              "Owner" : "rQ3fNyLjbvcDaPNS4EAJY8aT9zR3uGk17c",
              "RootIndex" : "07CE63F6E62E095CAF97BC77572A203D75ECB68219F97505AC5DF2DB061C9D96"
            },
            "LedgerEntryType" : "DirectoryNode",
            "LedgerIndex" : "07CE63F6E62E095CAF97BC77572A203D75ECB68219F97505AC5DF2DB061C9D96"
          }
        },
        {
          "CreatedNode" : {
            "LedgerEntryType" : "Offer",
            "LedgerIndex" : "2A4FA1778B70522863C0AEDC0B15D53315A52600014A73EEE687019BA74061D1",
            "NewFields" : {
              "Account" : "rQ3fNyLjbvcDaPNS4EAJY8aT9zR3uGk17c",
              "BookDirectory" : "623C4C4AD65873DA787AC85A0A1385FE6233B6DE100799474F06F3D3998F3D70",
              "Sequence" : 11898201,
              "TakerGets" : "7391586793",
              "TakerPays" : {
                "currency" : "CNY",
                "issuer" : "rJ1adrpGS3xsnQMb9Cw54tWJVFPuSdZHK",
                "value" : "14464.89186027806"
              }
            }
          }
        },
        {
          "ModifiedNode" : {
            "FinalFields" : {
              "Account" : "rQ3fNyLjbvcDaPNS4EAJY8aT9zR3uGk17c",
              "Balance" : "20342955901",
              "Flags" : 0,
              "OwnerCount" : 5,
              "Sequence" : 11898202
            },
            "LedgerEntryType" : "AccountRoot",
            "LedgerIndex" : "47FE64F9223D604034486F4DA7A175D5DA7F8A096952261CF8F3D77B74DC4AFA",
            "PreviousFields" : {
              "Balance" : "20342955913",
              "Sequence" : 11898201
            },
            "PreviousTxnID" : "66ACDC5B06B4FC7E80785D24520C9C9ABF15D17D15E08D669C64F2DA3CAC9352",
            "PreviousTxnLgrSeq" : 50047248
          }
        },
        {
          "DeletedNode" : {
            "FinalFields" : {
              "ExchangeRate" : "4F06F3A2B4A3177C",
              "Flags" : 0,
              "RootIndex" : "623C4C4AD65873DA787AC85A0A1385FE6233B6DE100799474F06F3A2B4A3177C",
              "TakerGetsCurrency" : "0000000000000000000000000000000000000000",
              "TakerGetsIssuer" : "0000000000000000000000000000000000000000",
              "TakerPaysCurrency" : "000000000000000000000000434E590000000000",
              "TakerPaysIssuer" : "0360E3E0751BD9A566CD03FA6CAFC78118B82BA0"
            },
            "LedgerEntryType" : "DirectoryNode",
            "LedgerIndex" : "623C4C4AD65873DA787AC85A0A1385FE6233B6DE100799474F06F3A2B4A3177C"
          }
        },
        {
          "CreatedNode" : {
            "LedgerEntryType" : "DirectoryNode",
            "LedgerIndex" : "623C4C4AD65873DA787AC85A0A1385FE6233B6DE100799474F06F3D3998F3D70",
            "NewFields" : {
              "ExchangeRate" : "4F06F3D3998F3D70",
              "RootIndex" : "623C4C4AD65873DA787AC85A0A1385FE6233B6DE100799474F06F3D3998F3D70",
              "TakerPaysCurrency" : "000000000000000000000000434E590000000000",
              "TakerPaysIssuer" : "0360E3E0751BD9A566CD03FA6CAFC78118B82BA0"
            }
          }
        },
        {
          "DeletedNode" : {
            "FinalFields" : {
              "Account" : "rQ3fNyLjbvcDaPNS4EAJY8aT9zR3uGk17c",
              "BookDirectory" : "623C4C4AD65873DA787AC85A0A1385FE6233B6DE100799474F06F3A2B4A3177C",
              "BookNode" : "0000000000000000",
              "Flags" : 0,
              "OwnerNode" : "0000000000000000",
              "PreviousTxnID" : "C76572742DC30A73B2173E04CEFB9469938CC973913366BC1AF92F566256281D",
              "PreviousTxnLgrSeq" : 50047247,
              "Sequence" : 11898197,
              "TakerGets" : "718800800",
              "TakerPays" : {
                "currency" : "CNY",
                "issuer" : "rJ1adrpGS3xsnQMb9Cw54tWJVFPuSdZHK",
                "value" : "1406.499090176633"
              }
            },
            "LedgerEntryType" : "Offer",
            "LedgerIndex" : "6C0749A52178CC0D88918419AD59E1530018A9B50D6A4A9AC2DD0C603919A0B1"
          }
        }
      ],
      "TransactionIndex" : 10,
      "TransactionResult" : "tesSUCCESS"
    },
    "owner_funds" : "20297955889"
  }     """

    val done = for {
      rs <- TestRegimes.testLedgerTxn(json)
    } yield rs
    getOrLog(done)

  }
}
