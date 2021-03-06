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

  def encodeAccountNoVL(json: Json): Either[BinCodecLibError, RawValue] = {
    for {
      accountChecksum <- json.asString.toRight(BCJsonErr("Account JSON Not String", json))
      _ = scribe.debug(s"Account w/ Checksum: ${accountChecksum}")
      decoded <- RippleBase58.decode(accountChecksum)
      unchecked = decoded.drop(1).dropRight(4)
      ubytesL   = unchecked.map(UByte(_)).toList

      res = ubytesL.length match {
        case len if len === 20 => ubytesL
        case len if len < 20   => List.fill(20 - ubytesL.length)(UByte(0)) ::: ubytesL
        case len if len > 20   => ubytesL.take(20)

      }
    } yield RawValue(res)

  }
}

object AccountIdCodecs extends AccountIdCodecs
