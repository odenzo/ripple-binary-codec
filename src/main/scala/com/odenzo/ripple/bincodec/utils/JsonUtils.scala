package com.odenzo.ripple.bincodec.utils

import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}

import com.odenzo.ripple.bincodec.utils.caterrors.AppError

trait JsonUtils extends CirceUtils {


  def findField(name: String, json: JsonObject): Either[AppError, Json] = {
    Either.fromOption(json(name), AppError(s"Field $name not found ", json.asJson))
  }


  def findFieldAsObject(name: String, json: JsonObject): Either[AppError, JsonObject] = {
    findField(name,json).flatMap(json2object)
  }

  
  def json2object(json: Json): Either[AppError, JsonObject] = {
    Either.fromOption(json.asObject, AppError("Expected JSON Object", json))
  }

  def json2array(json: Json): Either[AppError, List[Json]] = {
    Either.fromOption(json.asArray.map(_.toList), AppError("Expected JSON Array", json))
  }

  def json2string(json: Json): Either[AppError, String] = {
    Either.fromOption(json.asString, AppError("Expected JSON String", json))
  }

}

object JsonUtils extends JsonUtils
