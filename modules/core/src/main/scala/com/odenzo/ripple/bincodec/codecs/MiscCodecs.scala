package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._

import scodec.bits._
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.BinCodecLibError

trait MiscCodecs extends CodecUtils {

  def encodeTransactionType(name: String): Either[BinCodecLibError, ByteVector] =
    dd.getTransactionTypeMnemonic(name).flatMap(_.encoded)

  def encodeLedgerEntryType(name: String) = dd.getLedgerEntryMnemonic(name).flatMap(_.encoded)

  def encodeTxnResultType(name: String) = dd.getTxnResultMnemonic(name).flatMap(_.encoded)

  import io.circe.Json
  import scodec.bits.ByteVector

}

object MiscCodecs extends MiscCodecs
