package com.odenzo.ripple.bincodec.scodecs

import scodec.{Attempt, Codec}
import scodec.codecs.{utf8, _}
import scodec.bits._

object ScodecConstants {

  import cats.Show
  import scodec.bits._

  val pathSetAnother: Codec[Unit]  = constant(hex"FF") // indicates another path follows
  val pathSetEnd: Codec[Unit]      = constant(hex"00") // indicates the end of the PathSet
  val objDel: Codec[Unit]          = constant(hex"0F") // Object Delimeter in some packed fields forget which
  val objectEndMarker: Codec[Unit] = constant(hex"E1") // indicates end of object this is STObject not blob
  val arrDel: Codec[Unit]          = constant(hex"0D") // Array delimeter
  val arrayEndMarker: Codec[Unit]  = constant(hex"F1") // End of Array

  val base58RippleAlphabet: String = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  /** Packaged up zeroFiatAmount  */
  val rawEncodedZeroFiatAmount: Codec[Unit] = constant(hex"0x80".padRight(20))
  val xrpCurrencyCode: Codec[Unit]          = constant(hex"00".padLeft(20))

  // The raw hex form in json can be handled, but can't be XRP
  // This should be 24 bits, 'X'.ascii , 'R'.ascii, 'P'.ascii (UTF-8 equivalent in that range)
  val correctXrpHexCode: Attempt[BitVector] = utf8.encode("XRP")

  /** Standard XRP encoding, like an ISO **/
  val xrpHex: Codec[Unit] = constant(hex"0158415500000000C1F76FF6ECB0BAC600000000")

  /** Valid currency characters */
  val rippleCurrencyAlphabet: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    "0123456789" +
    "<>(){}[]|?!@#$%^&*"

}

object PathCodecs {
  val kZERO: ByteVector                   = hex"00"
  val kAddressStep: ByteVector            = hex"01"
  val kCurrencyStep: ByteVector           = hex"10"
  val kIssuerStep: ByteVector             = hex"20"
  val accountType: ByteVector             = hex"01"
  val currencyType: ByteVector            = hex"10"
  val issuerType: ByteVector              = hex"20"
  val accountPrefix: ByteVector           = accountType
  val currencyPrefix: ByteVector          = currencyType
  val issuerPrefix: ByteVector            = issuerType
  val currencyAndIssuerPrefix: ByteVector = currencyType | issuerType

}
