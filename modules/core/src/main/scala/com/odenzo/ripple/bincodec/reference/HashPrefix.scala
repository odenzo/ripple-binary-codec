package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import spire.math.{UByte, UInt}

import com.odenzo.ripple.bincodec.utils.ByteUtils

/** These are all Four Bytes Long with bottom byte 00  */
object HashPrefix {
  import scodec.Codec
  import scodec.bits._
  import scodec.codecs._
  import scodec.codecs.literals._
  val raw = hex"54584E00"

  // Unsigned Transaction is "53545800 + TxBlob sha512half
  // Unsigned Multisigned
  // Signed "54584E00 (both kinds I think)
  /** For after a signed (and? multisignd) txn is signed */
  val transactionID = constant(hex"54584E00")
  // For unsigned single signer txn
  val transaction = constant(hex"534E4400")
  // account state
  val accountStateEntry = constant(hex"4D4C4E00")
  // inner node in tree
  val innerNode = constant(hex"4D494E00")
  // ledger master data for signing
  val ledgerHeader = constant(hex"4C575200")

  /**  inner transaction to single signed, before signing */
  val transactionSig = constant(hex"53545800")

  // inner transaction to sign
  val transactionMultiSig = constant(hex"534D5400")
  // validation for signing
  val validation = constant(hex"56414C00")
  // proposal for signing
  val proposal = constant(hex"50525000")
  // payment channel claim
  val paymentChannelClaim = constant(hex"434C4D00")

//
//  val transactionID: UInt       = fromChar("TXN")
//  val txNode: UInt              = fromChar("SND")
//  val leafNode: UInt            = fromChar("MLN")
//  val innerNode: UInt           = fromChar("MIN")
//  val innerNodeV2: UInt         = fromChar("INR")
//  val ledgerMaster: UInt        = fromChar("LWR")
//  val txSign: UInt              = fromChar("STX")
//  val txMultiSign: UInt         = fromChar("SMT")
//  val validation: UInt          = fromChar("VAL")
//  val proposal: UInt            = fromChar("PRP")
//  val manifest: UInt            = fromChar("MAN")
//  val paymentChannelClaim: UInt = fromChar("CLM")

  import scodec.codecs._
  def fromCode(a: Char, b: Char, c: Char): ByteVector = ByteVector(a.toByte, b.toByte, c.toByte) << 8

  def fromStr(str: String): Codec[(String, Unit)] = {
    fixedSizeBytes(3, utf8) ~ fixedSizeBytes(1, constant(hex"00"))
  }

}
