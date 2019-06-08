package com.odenzo.ripple.bincodec.deserialize

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe._
import io.circe.syntax._
import spire.math.UByte

import com.odenzo.ripple.bincodec.RippleCodecAPI
import com.odenzo.ripple.bincodec.serializing.BinarySerializer
import com.odenzo.ripple.bincodec.serializing.BinarySerializer.FieldEncoded
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}


/** Development helper, not completed */
trait TxBlobBuster extends StrictLogging with JsonUtils with ByteUtils {


  /** Go through and break apart in encoded fields. Not a blob  */
  def deencode(txBlobHex:String) = {



  }


  def decomposeTxBlob(txJson: JsonObject, txBlob: String) = {

    logger.info(s"txJson IN: ${txJson.asJson.spaces4}")
    // Note the hash is not included in the TxBlob, but *after* signing the TxnSignature is.

    // Testing utility really. Serializing the transaction, and then start removing
    // all serialized fields to see what it left.
    // It is my assumption to test that the payload signed is TxBlob - TxnSignature
    // Also to check if I am missing some mandatory fields.
    var remainingBlob = txBlob

    // I think hash and TxnSignature are not in definitions...
    val serialized: Either[RippleCodecError, BinarySerializer.NestedEncodedValues] = RippleCodecAPI.binarySerialize(txJson)

    serialized.foreach{ fields =>
      fields.enclosed.foreach{
        case x: FieldEncoded => logger.info(s" Field: ${x.data.fi.fieldID.toHex} " + x.data.key)
        case other           => logger.info(s"OTHER: ${other.toHex}")
      }

      fields.encoded.foreach{ rev =>
        val enc = rev.toHex
        remainingBlob = excise(remainingBlob, enc)
      }
    }
    logger.info(s"Remaining Blob: $remainingBlob")

    // Lets try hashing the TxBlob as is (which Txn Prefix)
    // Go thru the encoded and remove the TxnSignature (74) from txblob
    val noTxSig: Either[RippleCodecError, List[BinarySerializer.Encoded]] = for {
      encoded ← serialized
      filtered = encoded.enclosed.flatMap{
        case field: FieldEncoded if field.data.key == "TxnSignature" ⇒ None
        case other                                                   ⇒ Some(other)
      }
    } yield filtered

    val rawBytes = noTxSig.map(list ⇒ list.foldLeft(List.empty[UByte])(_ ::: _.rawBytes))
    val updateBlob = rawBytes.map(ubytes2hex)
    updateBlob.foreach{ ub ⇒
      logger.info(s"Blob with no TxnSignature:\n $ub \n $txBlob")
      logger.info(s"Len ${ub.length} vs ${txBlob.length}")
    }


  }

  def excise(data: String, toCut: String) = {
    logger.debug(s"Cutting $toCut from $data")
    val res = data.replaceFirst(toCut, "")
    if (res == data) throw new IllegalStateException(s"$toCut not found in $data")
    res
  }
}
