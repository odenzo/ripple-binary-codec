package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.BCLibErr
import com.odenzo.ripple.bincodec.BinCodecLibError
import scodec.bits._

/** Deals with Blobs and Hashes and things that are plain hex encoded in Json */
trait JsonCodecs extends CodecUtils {

  import cats._
  import cats.data._
  import cats.implicits._
  import io.circe.Json

  /**
    * @param data Json and the FieldData, FieldData is redundant
    *
    * @return
    */
  def encodeVector256(data: Json): Either[BinCodecLibError, ByteVector] = {
    for {
      arr    <- json2array(data)
      arrTxt <- arr.traverse(json2string)
      res    <- TrivialCodecFn.encodeVector256(arrTxt)
    } yield res
  }

  import scodec.bits.ByteVector

  /** Encodes each element of an array as an STObject.  */
  def encodeSTArray(data: Json, isSigning: Boolean): Either[BinCodecLibError, ByteVector] = {
    import io.circe.syntax._
    scribe.debug(s"Encoding STArray ${data.spaces4}")
    val bvl = for {
      arr <- json2arrobj(data)
      vals <- arr.traverse {
        case jo if jo.size == 1 => STObjectCodec.encodeSTObject(jo.asJson, isSigning = isSigning)
        case jo =>
          import com.odenzo.ripple.bincodec.BCJsonErr
          BCJsonErr("Array wasnt all single field nested object", jo.asJson).asLeft
      }
    } yield (vals)
    bvl.map(_.reduce(_ ++ _))
  }

}

object JsonCodecs extends JsonCodecs
