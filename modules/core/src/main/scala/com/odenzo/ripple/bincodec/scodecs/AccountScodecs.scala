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

  val minNoMax = SizeBound.atMost(3)

  // Logic: First char is 'r' <-> first byte '0'
  // Checksum is last 4 bytes, not sure how many characters all the time
  // Sometimes binary encoding includes VL sometimes not.
  // Length id slways 20 padded as needed

  import RippleBase58Scodec._

  case class Address(b58Check: String)
  val str: String = "aa"

  /** In Base58 String format all accounts have r prefix and checkum */
  def encodecChecksummedAccount(checksummed: String): Attempt[BitVector] = {
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

  /** Binary encoded accounts are always exactly 160 bits, for round trip this this add tpye prefix and calculate checksum
    * Calculating checksum requires Crypo though so we skip for now */
  def decodeAccount: Codec[BitVector] = bitsStrict(160)

  def sha256(vector: ByteVector) = {
    val sha256: Array[Byte] = java.security.MessageDigest.getInstance("SHA-256").digest(vector.toArray)
    ByteVector(sha256)
  }

  val xrpAddressDecoder: Decoder[String] =
    bitsStrict(160)
      .asDecoder
      .map((bv: BitVector) => checksumAddress(bv))
      .emap[String](rippleB58Dec)

  def checksumAddress(bitv: BitVector): BitVector = {
    require(bitv.length == 20 * 8)
    val base = (hex"00" ++ bitv.bytes)
    val res  = base ++ (sha256(sha256(base)).take(4))
    res.bits
  }
}
