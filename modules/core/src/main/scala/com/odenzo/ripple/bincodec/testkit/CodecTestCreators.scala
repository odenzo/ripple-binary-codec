package com.odenzo.ripple.bincodec.testkit

import com.odenzo.ripple.bincodec.{EncodedSTObject, BinCodecLibError, RippleCodecAPI}
import java.security.MessageDigest

import com.odenzo.ripple.bincodec.utils.ByteUtils
import com.odenzo.ripple.bincodec.reference.HashPrefix
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._

trait CodecTestCreators extends Logging with RippleTestUtils {

  /** This is the standard way to test encoding of a tx_json for a signrs
    *
    * @param reply  The tope level object in reply (id level). Traverses to result and gets infso
    */
  def checkTxBlob(reply: JsonObject): Either[Throwable, Boolean] = {

    for {

      txjson        <- findTxJsonInReply(reply)
      kTxBlob       <- findTxBlobInReply(reply)
      txBlobEncoded <- RippleCodecAPI.binarySerialize(txjson)
      txBlobHex = txBlobEncoded.toHex
      passFail <- compareTxBlobs(kTxBlob, txBlobHex, txBlobEncoded)
    } yield passFail
  }

  /**
    *
    * @param jobj AN object with a hash field in it, tx_json or ledgertxn etc.
    * @return
    */
  def checkHash(jobj: JsonObject): Either[BinCodecLibError, JsonObject] = {
    val reply = jobj.asJson
    for {
      kHash <- findField("hash", jobj).flatMap(json2string)
      hash  <- createResponseHashHex(jobj)
      passed <- if (hash === kHash) jobj.asRight
      else {
        logger.warn(s"Hash Not Correct, Got vs Expected: \n$hash \n$kHash")
        RippleCodecAPI.binarySerialize(jobj).foreach(v => logger.warn(s"ENcoding Was ${v.show}"))
        BinCodecLibError(s"Hash Computation Mismatch \n$hash \n$kHash").asLeft
      }

    } yield passed
  }

  /** THis just calls it and checks completion and transcoding basically. */
  def checkSigningSerializationLamely(reply: JsonObject): Either[BinCodecLibError, Boolean] = {
    for {
      txjson         <- findTxJsonInReply(reply)
      fullBytes      <- RippleCodecAPI.serializedTxBlob(txjson)
      signedBytes    <- RippleCodecAPI.signingTxBlob(txjson)
      signedStrBytes <- RippleCodecAPI.signingTxBlob(txjson.asJson.noSpaces)
      signedEncoded  <- RippleCodecAPI.binarySerializeForSigning(txjson)
      passed <- if (signedBytes.sameElements(signedStrBytes) &&
                    signedBytes.sameElements(signedEncoded.toBytes) &&
                    !(fullBytes sameElements signedBytes)) {
        true.asRight
      } else {
        BinCodecLibError(s"Failed Signing Serialization Crap").asLeft
      }
    } yield passed
  }

  /**
    *
    * @param rsObj tx_json or a LedgerTxn, anything with a hash field I think. But hardcoded to tranasctionID prefix
    * @return
    */
  def createResponseHashHex(rsObj: JsonObject): Either[BinCodecLibError, String] = {
    BinCodecLibError.wrap("Creating Hash Failed") {
      RippleCodecAPI.binarySerialize(rsObj).map { serialized =>
        // logger.info(s"Raw:\n${rsObj.asJson.spaces4}")
        // logger.info(s"BinarySerialized for Hash:\n ${serialized.show}")
        val payload: Seq[Byte]    = HashPrefix.transactionID.asBytes ++ serialized.toBytes
        val digest: MessageDigest = MessageDigest.getInstance("SHA-512")
        digest.update(payload.toArray)
        val fullHash: Array[Byte] = digest.digest()
        val rHash                 = fullHash.take(32)
        ByteUtils.bytes2hex(rHash)
      }
    }
  }

  def compareTxBlobs(expected: String, got: String, encoded: EncodedSTObject): Either[BinCodecLibError, Boolean] = {
    (expected == got) match {
      case true =>
        logger.info("TxBlobs Matched")
        true.asRight
      case false =>
        logger.warn(s"Got  vs Expected Blob Len: ${got.length} and Got ${expected.length}")
        logger.warn(s"Got vs Expected Blob \n $got \n $expected")
        logger.warn(s"My Encoded Version:\n ${encoded.show}")
        for { // Now see what other details we can provide
          exEnc <- TxBlobBuster.bust(expected)
          _ = logger.info(s"TxBlob Expected Field: ${exEnc.show}")
          gotEnv <- TxBlobBuster.bust(got)
          _ = logger.info(s"TxBlob Got      Field: ${gotEnv.show}")

        } yield false
    }
  }

  def dumpEncoded(st: EncodedSTObject): Unit = {
    logger.warn(s"Encoded: ${st.show}")
  }
}

object CodecTestCreators extends CodecTestCreators
