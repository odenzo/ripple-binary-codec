package com.odenzo.ripple.bincodec.utils

import cats._
import cats.data._
import cats.implicits._
import io.circe.{JsonObject, ACursor}

private[bincodec] trait CirceCodecUtils {

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

private[bincodec] object CirceCodecUtils extends CirceCodecUtils