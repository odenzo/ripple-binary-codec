package com.odenzo.ripple.bincodec.testkit

import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.bincodec.ErrorOr.ErrorOr
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Json
import io.circe.JsonObject
import java.net.URL
import scala.io.BufferedSource
import scala.io.Source

import com.odenzo.ripple.bincodec.utils.JsonUtils

trait OTestUtils extends JsonUtils {

  /**
    * This will load from resources/test/fixtures/...
    * Most of those were stolen from Ripple Javascript.
    *
    * @param in  JSON File Name as input to a test fixture
    * @param out JSON File Name matching the desired result
    */
  def loadFixture(in: String, out: String): ErrorOr[(Json, Json)] = {

    for {
      inJson <- loadJsonResource(s"/test/fixtures/$in")
      okJson <- loadJsonResource(s"/test/fixtures/$out")
    } yield (inJson, okJson)

  }

  def loadRequestResponseFixture(path: String): Either[BinCodecLibError, List[JsonReqRes]] = {
    loadJsonResource(path).flatMap(json => decode(json, Decoder[List[JsonReqRes]]))
  }

  def loadJsonResource(path: String): Either[BinCodecLibError, Json] = {
    BinCodecLibError.handlingM(s"Getting Resource $path") {
      val resource: URL          = getClass.getResource(path)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }

  /** Usage {{{
    *
    * }}}
    */
  def fieldNameChangeEx(name: String, newName: String)(in: JsonObject): JsonObject = {
    // If missing existing name return JsonObject unchanges.
    // If oldname == null then i guess will add newName : null
    val updated: Option[JsonObject] = in(name)
      .map(oldVal => in.add(newName, oldVal))
      .map(jo => jo.remove(name))
    updated.getOrElse(in)
  }

  def changeObjectField(oldName: String, newName: String): ACursor => ACursor = {
    prepareJsonObject(fieldNameChangeEx(oldName, newName))
  }

  /** *
    * {{{
    *     val changer = fieldNameChangeEx("oldName","newName")
    *     Decoder[A].prepare(prepareJsonObject(changer))
    * }}}
    *
    * @param fn
    * @param in
    *
    * @return
    */
  def prepareJsonObject(fn: JsonObject => JsonObject)(in: ACursor): ACursor = {
    in.withFocus(json => json.mapObject(jobj => fn(jobj)))
  }

}

object OTestUtils extends OTestUtils
