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

/** How to deal with this I don't know yet, as its is really a control loop and Codec */
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
