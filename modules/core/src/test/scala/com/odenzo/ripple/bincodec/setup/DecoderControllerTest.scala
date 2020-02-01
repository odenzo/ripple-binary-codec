package com.odenzo.ripple.bincodec.setup

import com.odenzo.ripple.bincodec._
import io.circe.literal._
import _root_.scodec.bits._

class DecoderControllerTest extends OTestSpec {

  val txson =
    json"""  {
          "Account": "r4fwfyWgy25tjsb7YqMaRhBkanroZ7rDqr",
          "Amount": {
            "currency": "NZD",
            "issuer": "rsPEGecnKhQ3bkH3co1uRSD3tUwHDrDz1T",
            "value": "10"
          },
          "Destination": "rfDnjX6LYsv1Uo6RodimJy5KpQk543TKMT",
          "Fee": "50",
          "Flags": 0,
          "LastLedgerSequence": 4294967295,
          "Paths": [
            [
              {
                "account": "rGrg8a65omKD6F4moDXRiS82XMXtpM996Z",
                "type": 1,
                "type_hex": "0000000000000001"
              },
              {
                "currency": "NZD",
                "issuer": "rsPEGecnKhQ3bkH3co1uRSD3tUwHDrDz1T",
                "type": 48,
                "type_hex": "0000000000000030"
              }
            ]
          ],
          "SendMax": {
            "currency": "USD",
            "issuer": "rGrg8a65omKD6F4moDXRiS82XMXtpM996Z",
            "value": "20"
          },
          "Sequence": 3,
          "SigningPubKey": "EDF0B6D83E34724369D0B1064FE2B944843E8B5AF1EA0BBB889E0F70BEA2B3F771",
          "TransactionType": "Payment",
          "TxnSignature": "99B1A429D1B48E82EC97D3D1B1DA2268BD5EB53F42C33FE50494853310B6885B4DC7E969AC42F9E5F4D1F069D01CA4641B805DCB6B0FBC2BD9BA355F360FEA07",
          "hash": "2BA9A0A613E1C724A13EA92417A2907A5D6C6472FE2D30A209E20E8325BE8016"
        }
      """

  val txblox =
    hex"12000022000000002400000003201BFFFFFFFF61D4C38D7EA4C680000000000000000000000000004E5A4400000000001A255086B5137A6E57079B1B4FFF4F75C61B4F7F68400000000000003269D4C71AFD498D00000000000000000000000000005553440000000000A4AB176547A22ED23E6D8C3138780526830081D27321EDF0B6D83E34724369D0B1064FE2B944843E8B5AF1EA0BBB889E0F70BEA2B3F771744099B1A429D1B48E82EC97D3D1B1DA2268BD5EB53F42C33FE50494853310B6885B4DC7E969AC42F9E5F4D1F069D01CA4641B805DCB6B0FBC2BD9BA355F360FEA078114E784C01C1F30F461ED92E6CBC96A177AA8822DB283144419B287060B8112B323AF730A8B1DF254DD6470" ++
      hex"_0112_01_A4AB176547A22ED23E6D8C3138780526830081D2_30_0000000000000000000000004E5A440000000000_1A255086B5137A6E57079B1B4FFF4F75C61B4F7F_00"

  test("Decoding") {
    val foo = Setup.config.fields.length
    DecoderController.decode(txblox.bits).require
  }
}
