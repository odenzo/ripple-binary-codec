package com.odenzo.ripple.bincodec.scodecs

import io.circe.JsonObject
import scodec.{Codec, _}
import scodec.codecs._

import com.odenzo.ripple.bincodec.setup.Setup
import com.odenzo.ripple.bincodec.scodecs.BasicScodecs._

trait AdditionalScodecs {

  /*
 n addition to all of the above field types, the following types may appear in other contexts, such as ledger objects and transaction metadata:

Type Name	Type Code	Length-prefixed?	Description
Transaction	10001	No	A "high-level" type containing an entire transaction.
LedgerEntry	10002	No	A "high-level" type containing an entire ledger object.
Validation	10003	No	A "high-level" type used in peer-to-peer communications to represent a validation vote in the consensus process.
Metadata	10004	No	A "high-level" type containing metadata for one transaction.
UInt64	3	No	A 64-bit unsigned integer. This type does not appear in transaction instructions, but several ledger objects use fields of this type.
Vector256	19	Yes	This type does not appear in transaction instructions, but the Amendments ledger object's Amendments field uses this to represent which amendments are currently enabled.


   */

  val xrplTransactionType: Codec[String] = xrpuint16
    .xmap(num => Setup.getTransactionType(num), str => Setup.getTransactionTypeCode(str))
    .withContext("TransactionType")
    .withToString("TransactionType")

  val xrplLedgerEntryType: Codec[String] = xrpuint16
    .xmap(
      num => Setup.getLedgerEntryType(num),
      str => Setup.getLedgerEntryTypeCode(str)
    )
    .withContext("LedgerEntryType")
    .withToString("LedgerEntryType")

  val xrplValidation: Codec[JsonObject] = fail(Err(s"Validation Special Code Is Only for special cases :-)"))
}

object AdditionalScodecs extends AdditionalScodecs
