package com.odenzo.ripple.bincodec.testkit

import scribe.Logging
import cats._
import cats.data._
import cats.implicits._

trait CodecTestCreators extends Logging with RippleTestUtils {

//  def checkTxBlob(reply: Json) = {
//    import scodec.bits.ByteVector
//

//  def checkHash(jobj: Json): Either[BinCodecLibError, Json] = {
//
//    for {
//      kHash <- findField("hash", jobj).flatMap(json2string)
//      hash  <- createResponseHashHex(jobj)
//      passed <- if (hash === kHash) jobj.asRight
//      else {
//        import com.odenzo.ripple.bincodec.RippleCodecAPI
//        logger.warn(s"Hash Not Correct, Got vs Expected: \n$hash \n$kHash")
//        RippleCodecAPI.serializedTxBlob(jobj).foreach(v => logger.warn(s"ENcoding Was ${v}"))
//        BinCodecLibError(s"Hash Computation Mismatch \n$hash \n$kHash").asLeft
//      }
//
//    } yield passed
//  }
//  def createResponseHashHex(rsObj: Json): Either[BinCodecLibError, String] = {
//    BinCodecLibError.handlingM("Creating Hash Failed") {
//      import com.odenzo.ripple.bincodec.RippleCodecAPI
//      RippleCodecAPI.serializedTxBlob(rsObj).map { serialized =>
//        // logger.info(s"Raw:\n${rsObj.asJson.spaces4}")
//        // logger.info(s"BinarySerialized for Hash:\n ${serialized.show}")
//        import scodec.bits.ByteVector
//        val tx                    = ByteVector(serialized)
//        val payload: ByteVector   = HashPrefix.transactionID.encode(()).require.bytes ++ tx
//        val digest: MessageDigest = MessageDigest.getInstance("SHA-512")
//        digest.update(payload.toArray)
//        val fullHash: Array[Byte] = digest.digest()
//        val rHash                 = fullHash.take(32)
//        ByteVector(rHash).toHex
//      }
//    }
//  }

}

object CodecTestCreators extends CodecTestCreators
