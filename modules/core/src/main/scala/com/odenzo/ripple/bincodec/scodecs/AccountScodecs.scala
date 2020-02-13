package com.odenzo.ripple.bincodec.scodecs

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder}
import scodec.bits._
import scodec.codecs._

/**
  * Accounts are sometimes VL Encoded, although fixed length. These do not handle the VL Encoding or consume it.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
trait AccountScodecs {

  import RippleBase58Scodec._

  // Logic: First char is 'r' <-> first byte '0'
  // Checksum is last 4 bytes, not sure how many characters all the time
  // Sometimes binary encoding includes VL sometimes not.
  // Length id slways 20 padded as needed

  /** In Base58 String format all accounts have r prefix and checkum
    * This is variable length, someone will have to chuck out ahead of time */
  def accountEncoderFn(b58Check: String): Attempt[BitVector] = {
    val len = b58Check.length
    scribe.warn(s"Account Str: $b58Check ($b58Check.length)")
    require(len >= 30, "Account B58 Length < 30")
    require(len <= 35, "Account B58 Length > 35")
    require(b58Check.startsWith("r"), s"Address Must Start With 'r' but got ${b58Check}")

    val fullbin: Attempt[BitVector] = xrplBase58.encode(b58Check)
    fullbin.map(_.drop(8).dropRight(32))
  }

  val accountEncoder: Encoder[String] = Encoder(accountEncoderFn _)

  /** The 'r' and the checksum are not present in the binary format
    * This always takes exactly 160 bits s*/
  protected val accountDecoder: Decoder[String] = {
    bits(160)
      .asDecoder
      .map(checksumAddress)
      .emap(toRippleBase58)
  }

  /** This decodes 160 bits, it does not deal with VL Encoding at all */
  val xrplAccount: Codec[String] = Codec[String](accountEncoder, accountDecoder)
    .withContext("xrplAccount")

  /** Adds the leading 'r' and calculated 4 byte checksum, appending it to result */
  protected def checksumAddress(bitv: BitVector): BitVector = {
    scribe.info(s"Calculating Checksum Based on $bitv")
    val base         = (hex"00" ++ bitv.bytes) // Add the 'r'
    val withChecksum = base ++ (sha256(sha256(base)).take(4))
    scribe.debug(s"With CheckSum: $withChecksum")
    withChecksum.bits
  }

  private def sha256(vector: ByteVector) = {
    import java.security.MessageDigest
    val sha256: Array[Byte] = MessageDigest.getInstance("SHA-256").digest(vector.toArray)
    ByteVector(sha256)
  }

}

object AccountScodecs extends AccountScodecs
