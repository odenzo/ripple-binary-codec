package com.odenzo.ripple.bincodec.codecs

import cats.implicits._
import io.circe.Json
import spire.math.UByte

import com.odenzo.ripple.bincodec.utils.RippleBase58
import com.odenzo.ripple.bincodec.{EncodedVL, BCLibErr, RawValue, BinCodecLibError}

/**
  * Accounts a little special and also I want to create, encode, and decode later.
  * For now lets stick with encoding.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
trait AccountIdCodecs {

  /*
   * Quick Overview:
   *  An Address is typically Base64 encoded at API level with:
      - Between 25 and 35 characters in length
      -Starts with the character r
      -Uses alphanumeric characters, excluding the number "0" capital letter "O", capital letter "I", and lowercase
       letter "l"
      -Case-sensitive
      -Includes a 4-byte checksum so that the probability of generating a valid address from random characters is
   * approximately 1 in 2^32 The checksum seems to be the top four bytes after marker.

   *  - Sometimes account is VLEncoded, when nested (e.g. FiatAmount) it is not VLENcoded
   *  - Should always be 160 bits ( rrrrrrrrrrrrrrrrrrrrBZbvji is short one that gives problems)
   *

   */

  /**
    *  encodes an Account field with VL Header (variable length field)
    *  When not nested, this form is useed.
    */
  def encodeAccount(json: Json): Either[BCLibErr, EncodedVL] = {
    for {
      bits160 <- encodeAccountNoVL(json)
      vl      <- VLEncoding.encodeVL(bits160.rawBytes.length) // ALways 20 bytes
      fused = EncodedVL(vl, bits160)
    } yield fused

  }

  def encodeAccountNoVL(json: Json): Either[BCLibErr, RawValue] = {
    val account: Either[BCLibErr, String] =
      Either.fromOption(json.asString, BinCodecLibError("Account JSON Not String"))

    val asBytes: Either[BCLibErr, List[UByte]] = account.map { s =>
      val allBytes: List[UByte] = RippleBase58.decode(s).map(UByte(_)).toList
      val padded = if (allBytes.length < 24) {
        List.fill(24 - allBytes.length)(UByte(0)) ::: allBytes
      } else allBytes
      // Doh, padTo appends not prepends

      val ans: List[UByte] = padded.take(160 / 8)
      ans
    }
    asBytes.map(v => RawValue(v))
  }
}

object AccountIdCodecs extends AccountIdCodecs
