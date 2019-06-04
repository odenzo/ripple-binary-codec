package com.odenzo.ripple.bincodec.utils

import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.bincodec.utils.caterrors.CodecError

trait JsonUtils extends CirceUtils {


  def findField(name: String, json: JsonObject): Either[CodecError, Json] = {
    Either.fromOption(json(name), CodecError(s"Field $name not found ", json.asJson))
  }


  def findFieldAsObject(name: String, json: JsonObject): Either[CodecError, JsonObject] = {
    findField(name,json).flatMap(json2object)
  }

  
  def json2object(json: Json): Either[CodecError, JsonObject] = {
    Either.fromOption(json.asObject, CodecError("Expected JSON Object", json))
  }

  def json2array(json: Json): Either[CodecError, List[Json]] = {
    Either.fromOption(json.asArray.map(_.toList), CodecError("Expected JSON Array", json))
  }

  def json2string(json: Json): Either[CodecError, String] = {
    Either.fromOption(json.asString, CodecError("Expected JSON String", json))
  }

}

object JsonUtils extends JsonUtils
