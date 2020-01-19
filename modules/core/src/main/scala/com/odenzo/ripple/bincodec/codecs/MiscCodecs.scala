package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.BinCodecLibError

trait MiscCodecs extends CodecUtils {

  import scodec.bits.ByteVector

  import com.odenzo.ripple.bincodec.BCLibErr
  import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr

  /** Encodes the hex including the Variable Length info
    * This string must not be zero length string, or we maybe return a EmptyVal is it is.
    **/
  def encodeBlob(hex: String): ErrorOr[ByteVector] = {
    encodeHex(hex).flatMap(VLEncoding.prependVL)
  }

  /** Hex can have spaces and _ and optionally 0x in front, case ignored but migrate to uppercase as standard.*/
  def encodeHex(hex: String) = {
    import scodec.bits.Bases.Alphabets
    import scodec.bits.ByteVector
    ByteVector.fromHexDescriptive(hex.toUpperCase, Alphabets.HexUppercase).leftMap(BinCodecLibError(_))

  }

  def encodeTransactionType(name: String): Either[BCLibErr, ByteVector] =
    dd.getTransactionTypeMnemonic(name).map(_.encoded)

  def encodeLedgerEntryType(name: String): Either[BCLibErr, ByteVector] = dd.getLedgerEntryMnemonic(name).map(_.encoded)
  def encodeTxnResultType(name: String): Either[BCLibErr, ByteVector]   = dd.getTxnResultMnemonic(name).map(_.encoded)

}

object MiscCodecs extends MiscCodecs
