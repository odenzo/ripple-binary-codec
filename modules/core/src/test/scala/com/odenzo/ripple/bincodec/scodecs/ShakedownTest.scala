package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.setup.DecoderController
import io.circe.literal._
import scodec.bits._

class ShakedownTest extends OTestSpec {

  val master = json"""
   {
    "rq": {
      "command": "sign",
      "tx_json": {
        "TransactionType": "Payment",
        "Account": "rmPD5tJXdk3h4guoCsNADeDXRzmjvG3Ez",
        "Amount": "500",
        "Destination": "rL8igKxCefdw8Jnmp2W4wpmgKjTfu1seKA",
        "Sequence": 7,
        "Fee": "50000000",
        "Flags": 2147483648
      },
      "seed_hex": "A2B45386C3C79B35C74F99FDAE1F4771",
      "offline": false,
      "fee_multi_max": 1000,
      "key_type": "ed25519",
      "id": "c5d55871-c4ea-4281-9034-34b2e30ee915"
    },
    "rs": {
      "id": "c5d55871-c4ea-4281-9034-34b2e30ee915",
      "result": {
        "deprecated": "This command has been deprecated and will be removed in a future version of the server. Please migrate to a standalone signing tool.",
        "tx_blob": "120000228000000024000000076140000000000001F4684000000002FAF0807321EDFC71E719A57C9FAB8684234BE50B9C3C5AF07F99BD14E31F4CCEA4E9C3E6A9807440B874AE3DFE56DBCAF981C1872526B11CD72F7D27032F0D40A3993BA64C6FCB7A883EE1F77AA5CF28C6991523EACF5F5F1E3E179B39633E2FAC98A4703972E70881140864D99FE19C6B19B0B7BA865B9A4A552173A8968314D91693CB3D87723F716A16C46A398B9659864B0F",
        "tx_json": {
          "Account": "rmPD5tJXdk3h4guoCsNADeDXRzmjvG3Ez",
          "Amount": "500",
          "Destination": "rL8igKxCefdw8Jnmp2W4wpmgKjTfu1seKA",
          "Fee": "50000000",
          "Flags": 2147483648,
          "Sequence": 7,
          "SigningPubKey": "EDFC71E719A57C9FAB8684234BE50B9C3C5AF07F99BD14E31F4CCEA4E9C3E6A980",
          "TransactionType": "Payment",
          "TxnSignature": "B874AE3DFE56DBCAF981C1872526B11CD72F7D27032F0D40A3993BA64C6FCB7A883EE1F77AA5CF28C6991523EACF5F5F1E3E179B39633E2FAC98A4703972E708",
          "hash": "8847A1A0A1F577868467F7347781AE0DC40B05B40923564EC67A200058FF255B"
        }
      },
      "status": "success",
      "type": "rs"
    }
  }
  """

  val theBlob = hex"""
    1200002280000000
    2400000007
    6140000000000001F4
    684000000002FAF080
    7321EDFC71E719A57C9FAB8684234BE50B9C3C5AF07F99BD14E31F4CCEA4E9C3E6A980
    7440B874AE3DFE56DBCAF981C1872526B11CD72F7D27032F0D40A3993BA64C6FCB7A883EE1F77AA5CF28C6991523EACF5F5F1E3  E179B39633E2FAC98A4703972E708
  81140864D99FE19C6B19B0B7BA865B9A4A552173A896
  8314D91693CB3D87723F716A16C46A398B9659864B0F
  """

  test("Decode the Blob") {
    import BasicScodecs.xrpblob
    import STObjectScodec._
    DecoderController.decode(theBlob.bits)
  }

}
