package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.Json
import spire.math.{UByte, ULong}

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}
import com.odenzo.ripple.bincodec.{RawValue, BinCodecLibError, Encoded}

trait MiscCodecs extends CodecUtils {

  import scodec.bits.ByteVector

  import com.odenzo.ripple.bincodec.BCLibErr
  import com.odenzo.ripple.bincodec.reference.MnemonicType

  /** Encodes the hex including the Variable Length info
    * This string must not be zero length string, or we maybe return a EmptyVal is it is.
    **/
  def encodeBlob(hex: String): Either[BinCodecLibError, Encoded] = {
    encodeHex(hex).flatMap(VLEncoding.prependVL)
  }

  /** Hex can have spaces and _ and optionally 0x in front, case ignored but migrate to uppercase as standard.*/
  def encodeHex(hex: String) = {
    import scodec.bits.Bases.Alphabets
    import scodec.bits.ByteVector

    import com.odenzo.ripple.bincodec.BCLibErr
    ByteVector
      .fromHexDescriptive(hex.toUpperCase, Alphabets.HexUppercase)
      .leftMap(BinCodecLibError(_))

  }

  def encodeTransactionType(name: String): Either[BCLibErr, MnemonicType] = dd.getTransactionTypeMnemonic(name)
  def encodeLedgerEntryType(name: String): Either[BCLibErr, MnemonicType] = dd.getLedgerEntryMnemonic(name)
  def encodeTxnResultType(name: String): Either[BCLibErr, MnemonicType]   = dd.getTxnResultMnemonic(name)

}

object MiscCodecs extends MiscCodecs
