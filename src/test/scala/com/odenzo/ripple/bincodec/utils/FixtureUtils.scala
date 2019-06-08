package com.odenzo.ripple.bincodec.utils

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, Json, JsonObject}

import com.odenzo.ripple.bincodec.OTestSpec

trait FixtureUtils extends JsonUtils with OTestSpec {

  /** All fields in request are strings */
  def req2field(fix: JsonObject, fieldName: String): Option[String] = {
    fix("Request").flatMap(_.asObject).flatMap(v ⇒ v(fieldName)).flatMap(_.asString)
  }

  /** All fields in respone are strings */
  def res2field(fix: JsonObject, fieldName: String): Option[String] = {
    val result: Option[JsonObject] = fix("Response").flatMap(_.asObject).flatMap(v ⇒ v("result")).flatMap(_.asObject)
    val field                      = result.flatMap(v ⇒ v(fieldName))
    field.flatMap(_.asString)
  }

  def  findRequiredStringField(name:String,jobj:JsonObject): String = {
    getOrLog(findField(name,jobj).flatMap(json2string))
  }
  /**
    *
    * @return Json of field or logging of error and assertion failure
    */
  def findRequiredField(name: String, json: JsonObject): Json = {
     getOrLog(findField(name,json))
  }



  def findRequiredObject(name: String, jsonObject: JsonObject): JsonObject = {
    val asObj = findField(name, jsonObject).flatMap(json2object)
    getOrLog(asObj)
  }

  /**
    *
    * @param resource
    *
    * @return List of Json Requests and Responses tupled.
    */
  def loadTransactions(resource: String = "/test/Signing/secptxn.json"): List[(JsonObject, JsonObject)] = {

    val txnfixture: Json          = getOrLog(loadJsonResource(resource))
    val fixObjs: List[JsonObject] = getOrLog(JsonUtils.decode(txnfixture, Decoder[List[JsonObject]]))

    fixObjs.map { obj ⇒
      val req = findRequiredObject("Request", obj)
      val res = findRequiredObject("Response", obj)
      (req, res)
    }

  }


}
