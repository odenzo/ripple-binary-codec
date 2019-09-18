package com.odenzo.ripple.bincodec.codecs

import cats.implicits._
import io.circe.Json
import spire.math.UByte

import com.odenzo.ripple.bincodec.utils.{RippleBase58, ByteUtils}
import com.odenzo.ripple.bincodec.{BCJsonErr, BCLibErr, RawValue, EncodedVL, BinCodecLibError}

/**
  * Accounts a little special and also I want to create, encode, and decode later.
  * For now lets stick with encoding.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
trait AccountIdCodecs {

  /**
    *  encodes an Account field with VL Header (variable length field)
    *  When not nested, this form is useed.
    */
  def encodeAccount(json: Json): Either[BinCodecLibError, EncodedVL] = {
    for {
      bits160 <- encodeAccountNoVL(json)
      vl      <- VLEncoding.encodeVL(bits160.rawBytes.length) // ALways 20 bytes
    } yield EncodedVL(vl, bits160)
  }

  def encodeAccountNoVL(json: Json): Either[BCJsonErr, RawValue] = {

    val account = json.asString.toRight(BCJsonErr("Account JSON Not String", json))

    val asBytes: Either[BCJsonErr, List[UByte]] = account.map { s =>
      val allBytes = RippleBase58.decode(s).map(UByte(_)).toList

      // TODO: Add a generic padLeftTo somewhere
      val padded = if (allBytes.length < 24) {
        List.fill(24 - allBytes.length)(UByte(0)) ::: allBytes
      } else allBytes

      padded.take(160 / 8)

    }
    asBytes.map(v => RawValue(v))
  }
}

object AccountIdCodecs extends AccountIdCodecs
