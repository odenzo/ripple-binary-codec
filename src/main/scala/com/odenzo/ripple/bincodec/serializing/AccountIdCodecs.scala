package com.odenzo.ripple.bincodec.serializing

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.Json
import spire.math.UByte

import com.odenzo.ripple.bincodec.serializing.BinarySerializer.RawEncodedValue
import com.odenzo.ripple.bincodec.utils.RippleBase58
import com.odenzo.ripple.bincodec.utils.caterrors.{AppError, OError}

/**
  * Accounts a little special and also I want to create, encode, and decode later.
  * For now lets stick with encoding.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
object AccountIdCodecs extends StrictLogging {

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
    * Binary encodes an Account field (variable length field)
    *
    * @param json for the field value
    *
    * @return
    */
  def encodeAccount(json: Json): Either[OError, RawEncodedValue] = {
    // r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2
    // Spec is 160bits for account, but VLEncoded of length
    // To handle, make lenvth is 152 bits and VLEncoding is 8 bits
    //  VLLen  + account = 160 bits (20 bytes)

    // Is rrrrrrrrrrrrrrrrrrrrBZbvji a special case? All zeros in fixruew

    for {
      bits160 ← encodeAccountNoVL(json)
      vl      ← VLEncoding.encodeVL(bits160.rawBytes.length) // ALways 20 bytes
      fused   = RawEncodedValue(vl.ubytes ++ bits160.ubytes)
    } yield fused

  }

  /** In some cases, generally when inside another aboject like FiatAmount
    * the AccountID is 160 bits but is not VL encoded.
    * This is special case so stick with raw encoded value
    */
  def encodeAccountNoVL(json: Json): Either[OError, RawEncodedValue] = {
    // r4jQDHCUvgcBAa5EzcB1D8BHGcjYP9eBC2
    // Spec is 160bits for account, but VLEncoded of length
    // To handle, make lenvth is 152 bits and VLEncoding is 8 bits
    //  VLLen  + account = 160 bits (20 bytes)

    /*
      Between 25 and 35 characters in length
      Starts with the character r
      Uses alphanumeric characters, excluding the number "0" capital letter "O", capital letter "I", and lowercase letter "l"
      Case-sensitive
      Includes a 4-byte checksum so that the probability of generating a valid address from random characters is approximately 1 in 2^32

     */
    val account: Either[OError, String] = Either.fromOption(json.asString, AppError("Account JSON Not String"))
    val asBytes: Either[OError, List[UByte]] = account.map { s ⇒
      val allBytes: List[UByte] = RippleBase58.decode(s).map(UByte(_)).toList
      val padded = if (allBytes.length < 24) {
        List.fill(24 - allBytes.length)(UByte(0)) ::: allBytes
      } else allBytes

      logger.debug(s"Encoding $s produced ${allBytes.length}")
      val ans: List[UByte] = padded.take(160 / 8)
      assert(ans.length == 20, s"Account Encoded No VL Must be Exactly 20 bytes ${json.spaces2}")
      ans
    }
    asBytes.map(v ⇒ RawEncodedValue(v.toSeq))
  }

}
