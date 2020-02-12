package com.odenzo.ripple.bincodec.scodecs

/** These are all Four Bytes Long with bottom byte 00  */
object HashPrefixScodecs {

  import scodec.Codec
  import scodec.bits._
  import scodec.codecs._

  val raw = hex"54584E00"

  // Unsigned Transaction is "53545800 + TxBlob sha512half
  // Unsigned Multisigned
  // Signed "54584E00 (both kinds I think)
  /** For after a signed (and? multisignd) txn is signed */
  val transactionID: Codec[Unit] = constant(hex"54584E00")
  // For unsigned single signer txn
  val transaction: Codec[Unit] = constant(hex"534E4400")
  // account state
  val accountStateEntry: Codec[Unit] = constant(hex"4D4C4E00")
  // inner node in tree
  val innerNode: Codec[Unit] = constant(hex"4D494E00")
  // ledger master data for signing
  val ledgerHeader: Codec[Unit] = constant(hex"4C575200")

  /** inner transaction to single signed, before signing */
  val transactionSig: Codec[Unit] = constant(hex"53545800")

  // inner transaction to sign
  val transactionMultiSig: Codec[Unit] = constant(hex"534D5400")
  // validation for signing
  val validation: Codec[Unit] = constant(hex"56414C00")
  // proposal for signing
  val proposal: Codec[Unit] = constant(hex"50525000")
  // payment channel claim
  val paymentChannelClaim: Codec[Unit] = constant(hex"434C4D00")

  val kTransactionID: ByteVector       = byteVector("TXN")
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
      case _                  => throw new Exception("Could Not Initialize Hard Coded Transaction ID")
    }
  }

}
