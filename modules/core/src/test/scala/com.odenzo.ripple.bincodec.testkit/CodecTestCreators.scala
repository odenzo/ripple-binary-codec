package com.odenzo.ripple.bincodec.testkit

import com.odenzo.ripple.bincodec.BinCodecLibError
import java.security.MessageDigest

import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.reference.HashPrefix
import io.circe.Json
import scribe.Logging
import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._

trait CodecTestCreators extends Logging with RippleTestUtils {

  /** This is the standard way to test encoding of a tx_json for a signrs
    *
    * @param reply  The tope level object in reply (id level). Traverses to result and gets infso
    */
  def checkTxBlob(reply: Json) = {
    import scodec.bits.ByteVector

    import com.odenzo.ripple.bincodec.RippleCodecAPI

    for {
      txjson        <- findTxJsonInReply(reply)
      kTxBlob       <- findTxBlobInReply(reply)
      txBlobEncoded <- RippleCodecAPI.serializedTxBlob(txjson.asJson)
      txBlobHex = ByteVector(txBlobEncoded).toHex
    } yield (kTxBlob, txBlobEncoded)
  }

  /**
    *
    * @param jobj AN object with a hash field in it, tx_json or ledgertxn etc.
    * @return
    */
  def checkHash(jobj: Json): Either[BinCodecLibError, Json] = {

    for {
      kHash <- findField("hash", jobj).flatMap(json2string)
      hash  <- createResponseHashHex(jobj)
      passed <- if (hash === kHash) jobj.asRight
      else {
        import com.odenzo.ripple.bincodec.RippleCodecAPI
        logger.warn(s"Hash Not Correct, Got vs Expected: \n$hash \n$kHash")
        RippleCodecAPI.serializedTxBlob(jobj).foreach(v => logger.warn(s"ENcoding Was ${v}"))
        BinCodecLibError(s"Hash Computation Mismatch \n$hash \n$kHash").asLeft
      }

    } yield passed
  }

  /**
    *
    * @param rsObj tx_json or a LedgerTxn, anything with a hash field I think. But hardcoded to tranasctionID prefix
    * @return
    */
  def createResponseHashHex(rsObj: Json): Either[BinCodecLibError, String] = {
    BinCodecLibError.handlingM("Creating Hash Failed") {
      import com.odenzo.ripple.bincodec.RippleCodecAPI
      RippleCodecAPI.serializedTxBlob(rsObj).map { serialized =>
        // logger.info(s"Raw:\n${rsObj.asJson.spaces4}")
        // logger.info(s"BinarySerialized for Hash:\n ${serialized.show}")
        import scodec.bits.ByteVector
        val tx                    = ByteVector(serialized)
        val payload: ByteVector   = HashPrefix.transactionID.encode(()).require.bytes ++ tx
        val digest: MessageDigest = MessageDigest.getInstance("SHA-512")
        digest.update(payload.toArray)
        val fullHash: Array[Byte] = digest.digest()
        val rHash                 = fullHash.take(32)
        ByteUtils.bytes2hex(rHash)
      }
    }
  }

}

object CodecTestCreators extends CodecTestCreators
