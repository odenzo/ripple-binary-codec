package com.odenzo.ripple.bincodec.reference

import cats._
import cats.data._
import cats.implicits._
import spire.math.{UByte, UInt}

import com.odenzo.ripple.bincodec.utils.ByteUtils

/**
  * Ripple Hash Prefixes
  * @param v
  */
case class HashPrefix(v: List[UByte]) {

  val asHex: String       = ByteUtils.ubytes2hex(v)
  val asBytes: List[Byte] = v.map(_.toByte)
}

/** These are all Four Bytes Long with bottom byte 00  */
object HashPrefix {

  val raw: HashPrefix = fromHex("54584E00")

  // Unsigned Transaction is "53545800 + TxBlob sha512half
  // Unsigned Multisigned
  // Signed "54584E00 (both kinds I think)
  /** For after a signed (and? multisignd) txn is signed */
  val transactionID: HashPrefix = fromHex("54584E00")
  // For unsigned single signer txn
  val transaction: HashPrefix = fromHex("534E4400")
  // account state
  val accountStateEntry: HashPrefix = fromHex("4D4C4E00")
  // inner node in tree
  val innerNode: HashPrefix = fromHex("4D494E00")
  // ledger master data for signing
  val ledgerHeader: HashPrefix = fromHex("4C575200")

  /**  inner transaction to single signed, before signing */
  val transactionSig: HashPrefix = fromHex("53545800")

  // inner transaction to sign
  val transactionMultiSig: HashPrefix = fromHex("534D5400")
  // validation for signing
  val validation: HashPrefix = fromHex("56414C00")
  // proposal for signing
  val proposal: HashPrefix = fromHex("50525000")
  // payment channel claim
  val paymentChannelClaim: HashPrefix = fromHex("434C4D00")

  /** None of these overflow signed I think */
  def fromHex(lhex: String): HashPrefix = {
    val ubytes: List[UByte] = ByteUtils.unsafeHex2ubytes(lhex)
    scribe.warn(s"Long: $lhex bytes: $ubytes")
    HashPrefix(ubytes)
  }
//
//  val transactionID: UInt       = fromChar('T', 'X', 'N')
//  val txNode: UInt              = fromChar('S', 'N', 'D')
//  val leafNode: UInt            = fromChar('M', 'L', 'N')
//  val innerNode: UInt           = fromChar('M', 'I', 'N')
//  val innerNodeV2: UInt         = fromChar('I', 'N', 'R')
//  val ledgerMaster: UInt        = fromChar('L', 'W', 'R')
//  val txSign: UInt              = fromChar('S', 'T', 'X')
//  val txMultiSign: UInt         = fromChar('S', 'M', 'T')
//  val validation: UInt          = fromChar('V', 'A', 'L')
//  val proposal: UInt            = fromChar('P', 'R', 'P')
//  val manifest: UInt            = fromChar('M', 'A', 'N')
//  val paymentChannelClaim: UInt = fromChar('C', 'L', 'M')

  def fromChar(a: Char, b: Char, c: Char): UInt = {
    val aIn = UInt(a.toInt)
    val bIn = UInt(b.toInt) << 8
    val cIn = UInt(c.toInt) << 8

    val res: UInt = (aIn | bIn | cIn) << 8
    res
//
//    m_prefix = a
//    m_prefix = (m_prefix << 8) + b
//    m_prefix = (m_prefix << 8) + c
//    m_prefix = m_prefix << 8
  }
}
