package com.odenzo.ripple.bincodec.scodecs

import scodec.{Attempt, Codec}
import scodec.codecs._
import scodec.codecs.utf8
import scodec.bits._

import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.bincodec.codecs.PathCodecs
import com.odenzo.ripple.bincodec.reference.DefinitionData

object ScodecConstants {

  import cats.Show
  import scodec.bits.Bases.Alphabets
  import scodec.bits._

  val pathSetAnother  = constant(hex"FF") // indicates another path follows
  val pathSetEnd      = constant(hex"00") // indicates the end of the PathSet
  val objDel          = constant(hex"0F") // Object Delimeter in some packed fields forget which
  val objectEndMarker = constant(hex"E1") // indicates end of object this is STObject not blob
  val arrDel          = constant(hex"0D") // Array delimeter
  val arrayEndMarker  = constant(hex"F1") // End of Array

  val base58RippleAlphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

  /** Packaged up zeroFiatAmount  */
  val rawEncodedZeroFiatAmount = constant(hex"0x80".padRight(20))
  val xrpCurrencyCode          = constant(hex"00".padLeft(20))

  // The raw hex form in json can be handled, but can't be XRP
  // This should be 24 bits, 'X'.ascii , 'R'.ascii, 'P'.ascii (UTF-8 equivalent in that range)
  val correctXrpHexCode: Attempt[BitVector] = utf8.encode("XRP")

  /** Standard XRP encoding, like an ISO **/
  val xrpHex: ByteVector = constant(hex"0158415500000000C1F76FF6ECB0BAC600000000")

  /** Valid currency characters */
  val rippleCurrencyAlphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" +
    "0123456789" +
    "<>(){}[]|?!@#$%^&*"

}

object PathCodecs extends PathCodecs {
  val kZERO                    = hex"00"
  val kAddressStep             = hex"01"
  val kCurrencyStep            = hex"10"
  val kIssuerStep              = hex"20"
  val accountType: ByteVector  = hex"01"
  val currencyType: ByteVector = hex"10"
  val issuerType: ByteVector   = hex"20"
  val accountPrefix            = accountType
  val currencyPrefix           = currencyType
  val issuerPrefix             = issuerType
  val currencyAndIssuerPrefix  = currencyType | issuerType

  val anotherPathMarker: ByteVector = DefinitionData.pathSetAnother
  val endOfPathsMarker: ByteVector  = DefinitionData.pathSetEnd

}
