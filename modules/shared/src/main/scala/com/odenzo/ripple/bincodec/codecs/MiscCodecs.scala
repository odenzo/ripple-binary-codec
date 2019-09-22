package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.UByte

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{RawValue, BinCodecLibError, Encoded}

trait MiscCodecs extends CodecUtils with JsonUtils {

  /** Encodes the hex including the Variable Length info
    * This string must not be zero length string, or we maybe return a EmptyVal is it is.
    **/
  def encodeBlob(json: Json): Either[BinCodecLibError, Encoded] = {
    json2string(json).flatMap {
      case str if str.isEmpty => VLEncoding.prependVL(List.empty[UByte])
      case str                => ByteUtils.hex2ubytes(str).flatMap(VLEncoding.prependVL)
    }
  }

  protected def encodeMnemonicType(
      json: Json,
      lookupFn: String => Either[BinCodecLibError, Long]
  ): Either[BinCodecLibError, RawValue] = {
    for {
      str     <- json2string(json)
      ttype   <- lookupFn(str)
      encoded <- UIntCodecs.encodeUIntN(Json.fromLong(ttype), "UInt16")
    } yield encoded
  }

  def encodeTransactionType(json: Json): Either[BinCodecLibError, RawValue] = {
    encodeMnemonicType(json, dd.getTransactionType)
  }

  def encodeLedgerEntryType(json: Json): Either[BinCodecLibError, RawValue] = {
    encodeMnemonicType(json, dd.getLedgerEntryType)

  }
  def encodeTxnResultType(json: Json): Either[BinCodecLibError, RawValue] = {
    encodeMnemonicType(json, dd.getTxnResultType)
  }

}

object MiscCodecs extends MiscCodecs
