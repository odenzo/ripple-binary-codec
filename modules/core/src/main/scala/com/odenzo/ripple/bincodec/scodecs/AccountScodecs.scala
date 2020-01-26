package com.odenzo.ripple.bincodec.scodecs

import cats.implicits._
import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.codecs.implicits._

/**
  * Accounts a little special and also I want to create, encode, and decode later.
  * For now lets stick with encoding.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
trait AccountScodecs {

  import RippleBase58Scodec._

  val xrpaccount: Codec[String] = Codec(encodecChecksummedAccount _, decoderAttempt)

  //region Implementation

  val minNoMax = SizeBound.atMost(3)
  // Logic: First char is 'r' <-> first byte '0'
  // Checksum is last 4 bytes, not sure how many characters all the time
  // Sometimes binary encoding includes VL sometimes not.
  // Length id slways 20 padded as needed

  /** In Base58 String format all accounts have r prefix and checkum
    * This is variable length, someone will have to chuck out ahead of time */
  protected def encodecChecksummedAccount(checksummed: String): Attempt[BitVector] = {
    checksummed.length match {
      case s if s < 30 => Attempt.failure(Err(s"Address length $s < 30"))
      case s if s > 35 => Attempt.failure(Err(s"Address length $s > 36"))
      case s =>
        if (!checksummed.startsWith("r")) Attempt.failure(Err(s"Address length $s < 30"))
        else {
          rippleB58Enc(checksummed).map(_.drop(8).dropRight(32))
        }
    }
  }

  protected def sha256(vector: ByteVector) = {
    val sha256: Array[Byte] = java.security.MessageDigest.getInstance("SHA-256").digest(vector.toArray)
    ByteVector(sha256)
  }

  protected def decoderAttempt(bv: BitVector): Attempt[DecodeResult[String]] = {
    xrpAddressDecoder.decode(bv)
  }

  protected val xrpAddressDecoder: Decoder[String] =
    bitsStrict(160)
      .asDecoder
      .map((bv: BitVector) => checksumAddress(bv))
      .emap[String](rippleB58Dec)

  protected def checksumAddress(bitv: BitVector): BitVector = {
    require(bitv.length == 20 * 8)
    val base = (hex"00" ++ bitv.bytes)
    val res  = base ++ (sha256(sha256(base)).take(4))
    res.bits
  }
  //endregion
}

object AccountScodecs extends AccountScodecs
