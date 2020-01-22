package com.odenzo.ripple.bincodec.scodecs

import cats.implicits._
import scodec.bits.{BitVector, ByteVector}
import scodec._

/**
  * Accounts a little special and also I want to create, encode, and decode later.
  * For now lets stick with encoding.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
trait AccountScodecs {

  val minNoMax = SizeBound.atMost(3)

  // Logic: First char is 'r' <-> first byte '0'
  // Checksum is last 4 bytes, not sure how many characters all the time
  // Sometimes binary encoding includes VL sometimes not.
  // Length id slways 20 padded as needed
  val xrpAccountCheck: Codec[BitVector] = RippleBase58Scodec.xrpbase58

  val xrpAccount = xrpAccountCheck

  //val fixedVl = constant(VL.smallVL.encode(20))
//
//  /** Takes a Ripple Base58 Checksum and converts to binary */
//  def encodeAccountNoVL(accountChecksum: String): Either[BinCodecLibError, ByteVector] = {
//    for {
//      decoded <- RippleBase58.fromBase58Descriptive(accountChecksum.drop(1), RippleBase58Alphabet).leftMap(BinCodecLibError(_))
//      _        = scribe.debug(s"Account Raw Hex ${decoded.toHex(Alphabets.HexUppercase)}")
//      stripped = stripChecksum(decoded)
//      res = stripped.length match {
//        case len if len === 20 => stripped
//        case len if len < 20   => stripped.padLeft(20)
//        case len if len > 20   => stripped.take(20)
//      }
//    } yield res
//  }

  /** Strips the leading 0x00 (r) and the trailing four byte checksum */
  def stripChecksum(bv: ByteVector) = bv.drop(1).dropRight(4)
}
