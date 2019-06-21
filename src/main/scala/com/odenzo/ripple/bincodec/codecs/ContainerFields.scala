package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data.{Nested, _}
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import spire.math.UByte

import com.odenzo.ripple.bincodec.decoding.TxBlobBuster
import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.encoding.TypeSerializers.encodeFieldAndValue
import com.odenzo.ripple.bincodec.reference.{DefinitionData, FieldData, FieldInfo}
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.{OErrorRipple, RippleCodecError}
import com.odenzo.ripple.bincodec.{Decoded, DecodedField, DecodedNestedField, Encoded, EncodedDataType, EncodedField, EncodedNestedVals}

trait STObjectCodec extends StrictLogging with CodecUtils with JsonUtils {

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
  def encodeSTObject(o: Json, isNested: Boolean, isSigning: Boolean): Either[RippleCodecError, EncodedNestedVals] = {
    // Well, first lets try and get the fields and order them if needed.
    logger.info("Encoding STObject to Bytes")

    val jobj: Either[RippleCodecError, JsonObject] = json2object(o)

    val ans: Either[RippleCodecError, List[EncodedField]] = jobj
      .flatMap(prepareJsonObject(_, isSigning))
      .flatMap(lf ⇒ lf.traverse(encodeFieldAndValue(_, isNested, isSigning)))

    // Only at objectEndMarker when? Not for top level
    val optMarker = if (isNested) {
      ans.map((v: List[EncodedField]) => v :+ DefinitionData.objectEndMarker)
    } else ans

    optMarker.fmap(EncodedNestedVals)

  }

  def decodeSTObject(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (DecodedNestedField, List[UByte])] = {
    val endOfSTObjectMarker = UByte(0xE1)

    // info is for the top level array
    @scala.annotation.tailrec
    def subfields(ub: List[UByte], acc: List[Decoded]): Either[OErrorRipple, (List[Decoded], List[UByte])] = {
      if (ub.head === endOfSTObjectMarker) {
        (acc.reverse, ub.drop(1)).asRight
      } else {
        val next: Either[OErrorRipple, (Decoded, List[UByte])] = TxBlobBuster.decodeNextField(ub)
        logger.debug(s"STObject Next: $next")
        next match {
          case Left(err)            ⇒ err.asLeft
          case Right((field, tail)) ⇒ subfields(tail, field :: acc)
        }
      }
    }

    subfields(v, List.empty[Decoded]).fmap{
      case (fields, rest) ⇒
        (DecodedNestedField(info, fields), rest)
    }
  }

  /**
    *
    * Canonically sorts a json object and removes non-serializable or non-signing fields
    * TODO: Should just do this as a deep traverse once at the begining and avoid passing isSigning around.
    * @param o
    * @param isSigning Remove all non-signing fields if true, else serialized
    *
    * @return
    */
  def prepareJsonObject(o: JsonObject, isSigning: Boolean): Either[RippleCodecError, List[FieldData]] = {
    logger.trace(s"prepareJsonObect ")
    val bound: List[FieldData] = o.toList.flatMap {
      case (fieldName, fieldVal) ⇒ dd.optFieldData(fieldName, fieldVal)
    }
    val filtered = if (isSigning) {
      bound.filter(_.fi.isSigningField)
    } else {
      bound.filter(_.fi.isSerialized)
    }

    filtered.sortBy(_.fi.sortKey).asRight
  }

}

trait STArrayCodec extends StrictLogging with CodecUtils with JsonUtils {
  def encodeSTArray(data: FieldData, isSigning: Boolean): Either[RippleCodecError, EncodedNestedVals] = {
    logger.debug(s"STArray:\n${data.v.spaces2}")

    def handleOneVar(v: JsonObject, isSigning: Boolean): Either[RippleCodecError, EncodedField] = {
      v.toList match {
        case (fieldName: String, value: Json) :: Nil ⇒
          dd.getFieldData(fieldName, value)
            .flatMap(fd ⇒ encodeFieldAndValue(fd, isNestedObject = true, isSigning))

        case other ⇒ RippleCodecError("Expected Exaclty One Field", v.asJson).asLeft
      }
    }

    // TODO FIXME: Decide the best structure for this, nesting down or flattened.
    val arr    = json2array(data.v)
    val nested = Nested(arr).map(json2object).value.flatMap(_.sequence)

    val allAnwers: Either[RippleCodecError, List[EncodedField]] = nested.flatMap(_.traverse(handleOneVar(_, isSigning)))

    val noMetaData: Either[RippleCodecError, List[List[Encoded]]] =
      Nested(allAnwers).map(_.encoded).value

    val foo: Either[RippleCodecError, List[List[Encoded]]] =
      noMetaData.map((v: List[List[Encoded]]) => v :+ List(DefinitionData.arrayEndMarker))
    val bar: Either[RippleCodecError, List[EncodedNestedVals]] = Nested(foo).map(EncodedNestedVals).value
    bar.fmap((v: List[EncodedNestedVals]) ⇒ EncodedNestedVals(v))

  }

  def decodeSTArray(v: List[UByte], info: FieldInfo): Either[OErrorRipple, (DecodedNestedField, List[UByte])] = {
    val endOfFieldsMarker = UByte(0xF1)

    // info is for the top level array
    def subfields(ub: List[UByte], acc: List[Decoded]): Either[OErrorRipple, (List[Decoded], List[UByte])] = {
      if (ub.head === endOfFieldsMarker) {
        (acc.reverse, ub.drop(1)).asRight
      } else {
        val next: Either[OErrorRipple, (Decoded, List[UByte])] = TxBlobBuster.decodeNextField(ub)
        next match {
          case Left(err)            ⇒ err.asLeft
          case Right((field, tail)) ⇒ subfields(tail, field :: acc)
        }
      }
    }

    subfields(v, List.empty[DecodedField]).map{
      case (fields, rest) ⇒
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
  def encodeVector256(data: FieldData): Either[RippleCodecError, EncodedNestedVals] = {
// Like Indexes, these all seems to be 64 hex bytes in quotes 256 bits

    def encodeListOfHash(jsonList: List[Json]): Either[RippleCodecError, List[EncodedDataType]] =
      jsonList.traverse(HashHexCodecs.encodeHash256)

    val list = for {
      arr      ← json2array(data.v)
      hashes   ← encodeListOfHash(arr)
      totalLen = hashes.map(_.value.rawBytes.length).sum
      vl       ← VLEncoding.encodeVL(totalLen)

    } yield vl +: hashes
    list.map(EncodedNestedVals)
  }
}



object STArrayCodec extends STArrayCodec
object STObjectCodec extends STObjectCodec
object Vector256Codec extends Vector256Codec
object ContainerFields extends STArrayCodec with STObjectCodec with Vector256Codec
