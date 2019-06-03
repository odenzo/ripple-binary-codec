package com.odenzo.ripple.bincodec.serializing

import cats._
import cats.data.{Nested, _}
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.bincodec.reference.DefinitionData
import com.odenzo.ripple.bincodec.serializing.BinarySerializer.{Encoded, FieldData, FieldEncoded, NestedEncodedValues}
import com.odenzo.ripple.bincodec.serializing.TypeSerializers.{encodeFieldAndValue, encodeHash256, json2array, json2object}
import com.odenzo.ripple.bincodec.utils.caterrors.AppError

trait ContainerFields extends StrictLogging with SerializerUtils {

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
  def encodeSTObject(o: Json, isNested: Boolean, isSigning: Boolean): Either[AppError, NestedEncodedValues] = {
    // Well, first lets try and get the fields and order them if needed.
    logger.info("Encoding STObject to Bytes")

    // Bind each field to its metadata
    val jobj: Either[AppError, JsonObject] = json2object(o)

    // This will filter and order the fields
    val fields: Either[AppError, List[FieldData]] = jobj.flatMap(prepareJsonObject(_, isSigning))

    val ans: Either[AppError, List[FieldEncoded]] =
      fields.flatMap(lf ⇒ lf.traverse(encodeFieldAndValue(_, isNested, isSigning)))
    // Only at objectEndMarker when? Not for top level
   val optMarker = if (isNested) {
     ans.map((v: List[FieldEncoded]) => v :+ DefinitionData.objectEndMarker)
   } else ans
    optMarker.fmap(NestedEncodedValues)

  }

  /**
    *
    * Canonically sorts a json object and removes non-serializable or non-signing fields
    *
    * @param o
    * @param isSigning Remove all non-signing fields if true, else serialized
    *
    * @return
    */
  def prepareJsonObject(o: JsonObject, isSigning: Boolean): Either[AppError, List[FieldData]] = {
    logger.trace(s"prepareJsonObect ")
    val bound: Either[AppError, List[FieldData]] = o.toList.traverse {
      case (fieldName, fieldVal) ⇒ singleFieldData(fieldName, fieldVal)
    }

    val filtered =
      if (isSigning) Nested(bound).filter(_.fi.isSigningField).value
      else Nested(bound).filter(_.fi.isSerialized).value

    val sorted: Either[AppError, List[FieldData]] = filtered.map(_.sortBy(_.fi.sortKey))

    sorted
  }

  def encodeSTArray(data: FieldData, isSigning: Boolean): Either[AppError, NestedEncodedValues] = {
    logger.debug(s"STArray:\n${data.v.spaces2}")
    // This is just an (ordered or to be sorted?) list of fields in "MemoType": "Value" , each is serialized as
    // fieldheader + fieldValue based on the lookup of field name.
    /* Example:
     "Memos" : [
        {
            "Memo" : {
                "MemoType" : "584D4D2076616C7565",
                "MemoData" : "322E3230393635"
            }
        }
    ],
     */
    /**
      * This handles with fields are in, but need to check case when raw array or unnamed data.
      * For now, it just takes the first object (e.g. "Memo") and returns all those (fieldName,value)
      * @param v
      * @param isSigning If we are serializing for signing or all. Always nested here
      *
      * @return
      */
    def handleOneVar(v: JsonObject, isSigning: Boolean): Either[AppError, FieldEncoded] = {
      val asList = v.toList
      if (asList.length != 1) AppError("Expected Exaclty One Field", v.asJson).asLeft
      else {
        val (topFieldName: String, value: Json) = asList.head
        for {
          fieldData <- singleFieldData(topFieldName, value)
          bytes     ← encodeFieldAndValue(fieldData, isNestedObject = true, isSigning)
        } yield bytes
      }
    }

    // FIXME: Decide the best structure for this, nesting down or flattened.
    val arr: Either[AppError, List[Json]]                            = json2array(data.v)
    val nested: Either[AppError, List[Either[AppError, JsonObject]]] = Nested(arr).map(json2object).value
    val objArray: Either[AppError, List[JsonObject]]                 = nested.flatMap(_.sequence)

    val allAnwers: Either[AppError, List[FieldEncoded]] = objArray.flatMap(_.traverse(handleOneVar(_, isSigning)))

    val noMetaData: Either[AppError, List[List[BinarySerializer.Encoded]]] = Nested(allAnwers).map(_.encoded).value




    val foo: Either[AppError, List[List[Encoded]]]       = noMetaData.map((v: List[List[Encoded]]) => v :+ List(DefinitionData.arrayEndMarker))
    val bar: Either[AppError, List[NestedEncodedValues]] = Nested(foo).map(NestedEncodedValues).value
    bar.fmap((v: List[NestedEncodedValues]) ⇒ NestedEncodedValues(v))

  }


  /**
  *   This is usually a field, maybe always not sure.
    * @param data
    * @return
    */
  def encodeVector256(data: FieldData): Either[AppError, NestedEncodedValues] = {
    // Like Indexes, these all seems to be 64 hex bytes in quotes 256 bits

    def encodeListOfHash(jsonList: List[Json]): Either[AppError, List[BinarySerializer.RippleTypeEncoded]] =
      jsonList.traverse(encodeHash256)

    val list = for {
      arr      ← json2array(data.v)
      hashes   ← encodeListOfHash(arr)
      totalLen = hashes.map(_.value.rawBytes.length).sum
      vl       ← VLEncoding.encodeVL(totalLen)

    } yield vl +: hashes
    list.map(NestedEncodedValues)
  }

}

object ContainerFields extends ContainerFields
