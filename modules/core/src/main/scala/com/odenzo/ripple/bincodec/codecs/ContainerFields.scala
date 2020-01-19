package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe._
import io.circe.syntax._

import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.reference.DefinitionData
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait STObjectCodec extends CodecUtils with JsonUtils {

  import scodec.bits.ByteVector

  import com.odenzo.ripple.bincodec.reference.FieldData

  def encodeObject(o: Json, isSigning: Boolean): Either[BinCodecLibError, ByteVector] = {
    scribe.debug(s"Encoding STObject ${o.spaces4}")

    val endMarker = DefinitionData.objectEndMarker
    val byField = for {
      obj    <- prepareJsonObject(o, isSigning)
      fields <- obj.traverse(TypeSerializers.encodeFieldAndValue(_, signingModeOn = isSigning))
    } yield (fields)
    byField.map(_.reduce(_ ++ _))
  }

  /**
    * STObject encoding, but always with the end marker
    *
    * @param o
    * @param isSigning If true only signing fields serialized, or all serializable
    *                  This is determines from definitions.json
    *
    * @return List of each of the named fields in the object with their encoded information
    */
  def encodeSTObject(o: Json, isSigning: Boolean): Either[BinCodecLibError, ByteVector] = {
    import com.odenzo.ripple.bincodec.reference.RippleConstants
    encodeObject(o, isSigning).map(bv => bv ++ RippleConstants.objectEndMarker)
  }

  /**
    *
    * Canonically sorts a json object and removes non-serializable or non-signing fields
    * TODO: Should just do this as a deep traverse once at the begining and avoid passing isSigning around.
    *
    * @param j
    * @param isSigning Remove all non-signing fields if true, else serialized
    *
    * @return
    */
  def prepareJsonObject(j: Json, isSigning: Boolean): Either[BinCodecLibError, List[FieldData]] = {

    def filterFn(fd: FieldData): Boolean = if (isSigning) fd.fi.isSigningField else fd.fi.isSerialized

    for {
      jobj <- json2object(j.dropNullValues)
      bound    = jobj.toList.flatMap { case (fieldName, fieldVal) => dd.optFieldData(fieldName, fieldVal) }
      filtered = bound.filter(filterFn)
      sorted   = filtered.sortBy(_.fi.sortKey)
    } yield sorted
  }
}

/**
  * Kind of special case now until proven wrong. Array is always an array of objects.
  * Each object has exactly one named field. The field can be any type, including object or array.
  * Account is VLEncoded in here.
  */
trait STArrayCodec extends CodecUtils with JsonUtils {

  import scodec.bits.ByteVector

  /** Encodes each element of an array as an STObject.  */
  def encodeSTArray(data: Json, isSigning: Boolean): Either[BinCodecLibError, ByteVector] = {
    scribe.debug(s"Encoding STArray ${data.spaces4}")
    val bvl = for {
      arr <- json2arrobj(data)
      vals <- arr.traverse {
        case jo if jo.size == 1 => STObjectCodec.encodeSTObject(jo.asJson, isSigning = isSigning)
        case jo                 => BCJsonErr("Array wasnt all single field nested object", jo.asJson).asLeft
      }
    } yield (vals)
    bvl.map(_.reduce(_ ++ _))
  }

}

trait Vector256Codec extends CodecUtils with JsonUtils {

  import scodec.bits.ByteVector

  /**
    * @param data Json and the FieldData, FieldData is redundant
    *
    * @return
    */
  def encodeVector256(data: Json): Either[BinCodecLibError, ByteVector] = {

    val bvl = for {
      arr    <- json2array(data)
      arrTxt <- arr.traverse(json2string)
      hashes <- arrTxt.traverse(HashHexCodecs.encodeHash256)
      // Pendantic as we know the length for each really.
      totalLen = hashes.map(_.length).sum
      vl <- VLEncoding.encodeVL(totalLen.toInt)
    } yield (vl :: hashes)

    bvl.map(_.reduce(_ ++ _))
  }
}

object STArrayCodec extends STArrayCodec

object STObjectCodec extends STObjectCodec

object Vector256Codec extends Vector256Codec

object ContainerFields extends STArrayCodec with STObjectCodec with Vector256Codec
