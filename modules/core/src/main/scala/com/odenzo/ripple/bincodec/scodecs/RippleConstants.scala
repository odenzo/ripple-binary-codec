package com.odenzo.ripple.bincodec.scodecs

import scodec.codecs.utf8

object RippleConstants {

  import cats.Show
  import scodec.bits._

  val pathSetAnother  = hex"FF" // indicates another path follows
  val pathSetEnd      = hex"00" // indicates the end of the PathSet
  val objDel          = hex"0F" // Object Delimeter in some packed fields forget which
  val objectEndMarker = hex"E1" // indicates end of object this is STObject not blob
  val arrDel          = hex"0D" // Array delimeter
  val arrayEndMarker  = hex"F1" // End of Array

  final val objectMarkerEndName: String = "ObjectEndMarker"
  final val arrayMarkerEndName: String  = "ArrayEndMarker"

  /** Maximum XRP amount expressed in Drops, which requires only 7 bytes so safe in a Long  */
  val maxDrops: BigInt = spire.math.pow(BigInt(10), BigInt(17))

  /** Packaged up zeroFiatAmount  */
  val rawEncodedZeroFiatAmount: ByteVector = hex"0x80".padRight(20)

  val xrpCurrencyCode: ByteVector = hex"00".padLeft(20)

  // The raw hex form in json can be handled, but can't be XRP
  // This should be 24 bits, 'X'.ascii , 'R'.ascii, 'P'.ascii (UTF-8 equivalent in that range)
  val correctXrpHexCode: ByteVector = utf8.encode("XRP").require.bytes

  /** Standard XRP encoding, like an ISO **/
  val xrpHex: ByteVector = hex"0158415500000000C1F76FF6ECB0BAC600000000"

}
