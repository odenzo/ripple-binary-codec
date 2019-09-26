package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Json, JsonObject}
import spire.math.UByte

import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.encoding.{TypeSerializers, CodecUtils}
import com.odenzo.ripple.bincodec.reference.{FieldData, FieldMetaData, DefinitionData}
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait STObjectCodec extends CodecUtils with JsonUtils {

  /**
    * Top level object has no matching FieldInfo :-/
    *
    * @param o
    * @param isSigning If true only signing fields serialized, or all serializable
    *                  This is determines from definitions.json
    *
    * @return List of each of the named fields in the object with their encoded information
    */
  def encodeSTObject(o: Json, isSigning: Boolean): Either[BinCodecLibError, EncodedSTObject] = {
    scribe.debug(s"Encoding STObject ${o.spaces4}")
    for {
      obj    <- prepareJsonObject(o, isSigning)
      fields <- obj.traverse(TypeSerializers.encodeFieldAndValue(_, signingModeOn = isSigning))
    } yield EncodedSTObject(fields, isTopLevel = false)
  }

  def decodeSTObject(v: List[UByte], info: FieldMetaData): Either[BCLibErr, (DecodedNestedField, List[UByte])] = {
    val stObjectEndByte = UByte(0xE1)

    // info is for the top level array
    @scala.annotation.tailrec
    def subfields(ub: List[UByte], acc: List[Decoded]): Either[BCLibErr, (List[Decoded], List[UByte])] = {
      ub match {
        case h :: t if h === stObjectEndByte => (acc.reverse, ub.drop(1)).asRight
        case Nil                             => BinCodecLibError("Badly Formed STObject").asLeft
        case other => // The next bytes have another field
          TxBlobBuster.decodeNextField(ub) match {
            case Left(err)            => err.asLeft
            case Right((field, tail)) => subfields(tail, field :: acc)
          }
      }
    }

    subfields(v, List.empty[Decoded]).fmap {
      case (fields, rest) => (DecodedNestedField(info, fields), rest)
    }
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

  /** Encodes each element of an array as an STObject.  */
  def encodeSTArray(data: Json, isSigning: Boolean): Either[BinCodecLibError, EncodedSTArray] = {
    scribe.debug(s"Encoding STArray ${data.spaces4}")
    for {
      arr <- json2array(data)
      // Meh, a nasty hack because we do not want an End of Object Marker on the top level object in the array
      // Before I unwrapped, on basis that all STArrays are an array of oobject.
      // Not sure why I changed. Could check if top level objects have just one field
      //   _ = arr.forall(sj => sj.asObject.map(_.size)
      vals <- arr.traverse { j =>
        if (j.asObject.map(_.size) === Some(1))
          STObjectCodec.encodeSTObject(j, isSigning = isSigning).map(_.copy(isTopLevel = true))
        else {
          BCJsonErr("Array wasnt all single field nested object", j).asLeft
        }
      }
    } yield EncodedSTArray(vals)

  }

  def decodeSTArray(v: List[UByte], info: FieldMetaData): Either[BCLibErr, (DecodedNestedField, List[UByte])] = {
    val endOfFieldsMarker = UByte(0xF1)

    // info is for the top level array
    @scala.annotation.tailrec
    def subfields(ub: List[UByte], acc: List[Decoded]): Either[BCLibErr, (List[Decoded], List[UByte])] = {
      if (ub.head === endOfFieldsMarker) {
        (acc.reverse, ub.drop(1)).asRight
      } else {
        val next: Either[BCLibErr, (Decoded, List[UByte])] = TxBlobBuster.decodeNextField(ub)
        next match {
          case Left(err)            => err.asLeft
          case Right((field, tail)) => subfields(tail, field :: acc)
        }
      }
    }

    subfields(v, List.empty[DecodedField]).map { case (fields, rest) => (DecodedNestedField(info, fields), rest) }
  }
}

trait Vector256Codec extends CodecUtils with JsonUtils {

  /**
    * @param data Json and the FieldData, FieldData is redundant
    *
    * @return
    */
  def encodeVector256(data: Json): Either[BinCodecLibError, EncodedVector256] = {

    for {
      arr    <- json2array(data)
      hashes <- arr.traverse(HashHexCodecs.encodeHash256)
      // Pendantic as we know the length for each really.
      totalLen = hashes.map(_.value.rawBytes.length).sum
      vl <- VLEncoding.encodeVL(totalLen)
    } yield EncodedVector256(vl, hashes)

  }
}

object STArrayCodec extends STArrayCodec

object STObjectCodec extends STObjectCodec

object Vector256Codec extends Vector256Codec

object ContainerFields extends STArrayCodec with STObjectCodec with Vector256Codec
