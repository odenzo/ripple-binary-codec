package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.BinCodecLibError

/** These are all Four Bytes Long with bottom byte 00  */
object HashPrefixScodecs {

  import scodec.Codec
  import scodec.bits._
  import scodec.codecs._
  import spire.math.UInt

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

  /** inner transaction to single signed, before signing */
  val transactionSig = constant(hex"53545800")

  // inner transaction to sign
  val transactionMultiSig = constant(hex"534D5400")
  // validation for signing
  val validation = constant(hex"56414C00")
  // proposal for signing
  val proposal = constant(hex"50525000")
  // payment channel claim
  val paymentChannelClaim = constant(hex"434C4D00")

  val kTransactionID                   = byteVector("TXN")
  val kTxNode: ByteVector              = byteVector("SND")
  val kLeafNode: ByteVector            = byteVector("MLN")
  val kInnerNode: ByteVector           = byteVector("MIN")
  val kInnerNodeV2: ByteVector         = byteVector("INR")
  val kLedgerMaster: ByteVector        = byteVector("LWR")
  val kTxSign: ByteVector              = byteVector("STX")
  val kTxMultiSign: ByteVector         = byteVector("SMT")
  val kValidation: ByteVector          = byteVector("VAL")
  val kProposal: ByteVector            = byteVector("PRP")
  val kManifest: ByteVector            = byteVector("MAN")
  val kPaymentChannelClaim: ByteVector = byteVector("CLM")

  import scodec.codecs._

  def fromCode(a: Char, b: Char, c: Char): ByteVector = ByteVector(a.toByte, b.toByte, c.toByte) << 8

  def fromStr(str: String): Codec[(String, Unit)] = {
    fixedSizeBytes(3, utf8) ~ fixedSizeBytes(1, constant(hex"00"))
  }

  private def byteVector(str: String) = {
    str.toList match {
      case a :: b :: c :: Nil => fromCode(a, b, c)
      case _ =>
        throw BinCodecLibError("Could Not Initialize Hard Coded Transaction ID")
    }
  }

}
