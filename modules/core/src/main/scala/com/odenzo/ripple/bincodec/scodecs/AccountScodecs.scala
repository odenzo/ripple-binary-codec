package com.odenzo.ripple.bincodec.scodecs

import cats.implicits._
import scodec.bits._
import scodec._
import scodec.codecs._

/**
  * Accounts a little special and also I want to create, encode, and decode later.
  * For now lets stick with encoding.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
trait AccountScodecs {

  import RippleBase58Scodec._

  //region Implementation

  val minNoMax = SizeBound.atMost(3)
  // Logic: First char is 'r' <-> first byte '0'
  // Checksum is last 4 bytes, not sure how many characters all the time
  // Sometimes binary encoding includes VL sometimes not.
  // Length id slways 20 padded as needed

  /** In Base58 String format all accounts have r prefix and checkum
    * This is variable length, someone will have to chuck out ahead of time */
  protected val accountEncoder: Encoder[String] = {
    ascii.map { str: String =>
      val len = str.length
      if (!(len >= 30 && len <= 35)) throw new Exception(s"Address length $len  not between 30 and 35 inclusive")
      if (!str.startsWith("r")) throw new Exception(s"Address Must Start With 'r' but got ${str}")
      val fullbin: Attempt[BitVector] = rippleB58Enc(str)
      fullbin.map(_.drop(8).dropRight(32))
    }
  }.asEncoder

  protected val accountDecoder: Decoder[String] = {
    scodec
      .codecs
      .bits(160)
      .asDecoder
      .map((bv: BitVector) => checksumAddress(bv))
      .emap[String](rippleB58Dec)
  }

  /** This decodes 160 bits, it does not deal with VL Encoding at all */
  val xrpaccount: Codec[String] = Codec[String](accountEncoder, accountDecoder).withContext("xrpaccount")

  /** Adds the leading 'r' and calculated 4 byte checksum, appending it to result */
  protected def checksumAddress(bitv: BitVector): BitVector = {
    scribe.info(s"Checksum Address from BitV Len ${bitv.length}")
    (bitv.length == 20 * 8)
    val base = (hex"00" ++ bitv.bytes)
    val res  = base ++ (sha256(sha256(base)).take(4))
    res.bits
  }

  protected def sha256(vector: ByteVector) = {
    val sha256: Array[Byte] = java.security.MessageDigest.getInstance("SHA-256").digest(vector.toArray)
    ByteVector(sha256)
  }

  //endregion
}

object AccountScodecs extends AccountScodecs
