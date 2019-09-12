package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Json, JsonObject}
import spire.math.UByte

import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.encoding.{TypeSerializers, CodecUtils}
import com.odenzo.ripple.bincodec.reference.{FieldData, FieldMetaData}
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait STObjectCodec extends CodecUtils with JsonUtils {

  /**
    * Top level object has no matching FieldInfo :-/
    *
    * @param o
    * @param isNested  If a nested object somethings are serialized differenty.
    * @param isSigning If true only signing fields serialized, or all serializable
    *                  This is determines from definitions.json
    *
    * @return List of each of the named fields in the object with their encoded information
    */
  def encodeSTObject(o: Json, isNested: Boolean, isSigning: Boolean): Either[BinCodecLibError, EncodedSTObject] = {
    // Well, first lets try and get the fields and order them if needed.

    scribe.debug(s"Encoding STObject to Bytes: nested $isNested  Signing $isSigning")

    val jobj: Either[BinCodecLibError, JsonObject] = json2object(o.dropNullValues)

    val ans: Either[BinCodecLibError, List[EncodedField]] = jobj
      .flatMap(prepareJsonObject(_, isSigning))
      .flatMap(
        lf =>
          lf.traverse(
            TypeSerializers
              .encodeFieldAndValue(_, true, isSigning)
          )
      )

    // Only at objectEndMarker when? Not for top level
    ans.fmap(EncodedSTObject(_, isNested))

  }

  def decodeSTObject(v: List[UByte], info: FieldMetaData): Either[BCLibErr, (DecodedNestedField, List[UByte])] = {
    val endOfSTObjectMarker = UByte(0xE1)

    // info is for the top level array
    @scala.annotation.tailrec
    def subfields(ub: List[UByte], acc: List[Decoded]): Either[BCLibErr, (List[Decoded], List[UByte])] = {
      ub match {
        case h :: t if h === endOfSTObjectMarker => (acc.reverse, ub.drop(1)).asRight
        case Nil                                 => BinCodecLibError("Badly Formed STObject").asLeft
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
    * @param o
    * @param isSigning Remove all non-signing fields if true, else serialized
    *
    * @return
    */
  def prepareJsonObject(o: JsonObject, isSigning: Boolean): Either[BinCodecLibError, List[FieldData]] = {

    val bound: List[FieldData] = o.toList.flatMap {
      case (fieldName, fieldVal) => dd.optFieldData(fieldName, fieldVal)
    }
    val filtered = if (isSigning) {
      bound.filter(_.fi.isSigningField)
    } else {
      bound.filter(_.fi.isSerialized)
    }

    filtered.sortBy(_.fi.sortKey).asRight
  }

}

/**
  * Kind of special case now until proven wrong. Array is always an array of objects.
  * Each object has exactly one named field. The field can be any type, including object or array.
  * Account is VLEncoded in here.
  */
trait STArrayCodec extends CodecUtils with JsonUtils {
  def encodeSTArray(data: FieldData, isSigning: Boolean): Either[BinCodecLibError, EncodedSTArray] = {
    scribe.info(s"STArray:\n${data.json.spaces2}")

    def handleOneArrayElement(v: Json): Either[BinCodecLibError, EncodedSTObject] = {
      STObjectCodec.encodeSTObject(v, isNested = false, isSigning = isSigning)

    }

    // Sticking to one encoded field per array elem. Even though each field is actually nested in an object.
    val objects = json2array(data.json).flatMap(lj => lj.traverse(j => handleOneArrayElement(j)))
    val array   = objects.map(fields => EncodedSTArray(fields))
    array
  }

  def decodeSTArray(v: List[UByte], info: FieldMetaData): Either[BCLibErr, (DecodedNestedField, List[UByte])] = {
    val endOfFieldsMarker = UByte(0xF1)

    // info is for the top level array
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

    subfields(v, List.empty[DecodedField]).map {
      case (fields, rest) =>
        (DecodedNestedField(info, fields), rest)
    }
  }
}

trait Vector256Codec extends CodecUtils with JsonUtils {

  /**
    * This is usually a field, maybe always not sure.
    *
    * @param data
    *
    * @return
    */
  def encodeVector256(data: FieldData): Either[BinCodecLibError, EncodedVector256] = {
    // Like Indexes, these all seems to be 64 hex bytes in quotes 256 bits

    def encodeListOfHash(jsonList: List[Json]): Either[BinCodecLibError, List[EncodedDataType]] =
      jsonList.traverse(HashHexCodecs.encodeHash256)

    for {
      arr    <- json2array(data.json)
      hashes <- encodeListOfHash(arr)
      totalLen = hashes.map(_.value.rawBytes.length).sum
      vl <- VLEncoding.encodeVL(totalLen)
    } yield EncodedVector256(vl, hashes)

  }
}

object STArrayCodec extends STArrayCodec

object STObjectCodec extends STObjectCodec

object Vector256Codec extends Vector256Codec

object ContainerFields extends STArrayCodec with STObjectCodec with Vector256Codec
