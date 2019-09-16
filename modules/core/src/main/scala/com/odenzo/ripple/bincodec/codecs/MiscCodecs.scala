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

  def encodeTransactionType(json: Json): Either[BinCodecLibError, RawValue] = {
    json2string(json)
      .flatMap(dd.getTransactionType)
      .flatMap(l => UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16")) // PreBake
  }

  def encodeLedgerEntryType(json: Json): Either[BinCodecLibError, RawValue] = {
    json2string(json)
      .flatMap(dd.getLedgerEntryType)
      .flatMap(l => UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16")) // PreBake
  }

  def encodeTxnResultType(json: Json): Either[BinCodecLibError, RawValue] = {
    json2string(json)
      .flatMap(dd.getTxnResultType)
      .flatMap(l => UIntCodecs.encodeUIntN(Json.fromLong(l), "UInt16")) // PreBake
  }

}

object MiscCodecs extends MiscCodecs
