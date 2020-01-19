package com.odenzo.ripple.bincodec.codecs

import cats.implicits._
import io.circe.Json

import com.odenzo.ripple.bincodec.utils.JsonUtils._
import com.odenzo.ripple.bincodec.utils.RippleBase58
import com.odenzo.ripple.bincodec.BinCodecLibError

/**
  * Accounts a little special and also I want to create, encode, and decode later.
  * For now lets stick with encoding.
  * [[[https://developers.ripple.com/accounts.html#address-encoding]] is good reference point.
  */
trait AccountIdCodecs {

  import scodec.bits.ByteVector

  /**
    *  encodes an Account field with VL Header (variable length field)
    *  When not nested, this form is useed.
    */
  def encodeAccount(json: Json): Either[BinCodecLibError, ByteVector] = {
    for {
      bits160 <- encodeAccountNoVL(json)
      vl      <- VLEncoding.encodeVL(bits160.length.toInt) // ALways 20 bytes so could hard code
    } yield (vl ++ bits160)
  }

  def encodeAccountNoVL(json: Json): Either[BinCodecLibError, ByteVector] = {
    import scodec.bits.ByteVector
    for {
      accountChecksum <- json2string(json)
      decoded         <- RippleBase58.decode2bytes(accountChecksum)
      bv        = ByteVector(decoded)
      unchecked = bv.drop(1).dropRight(4) // Remove the leading marker (which is bits) and checksum
      res = unchecked.length match {
        case len if len === 20 => unchecked
        case len if len < 20   => unchecked.padLeft(20)
        case len if len > 20   => unchecked.take(20)
      }
    } yield res
  }
}

object AccountIdCodecs extends AccountIdCodecs
