package com.odenzo.ripple.bincodec.testkit

import cats._
import cats.data._
import cats.implicits._
import io.circe.optics.JsonPath.root
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax._
import monocle.Optional
import scribe.Logging

import com.odenzo.ripple.bincodec.BCJsonErr
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait RippleTestUtils extends Logging with JsonUtils {

  /** Apply an Optional lens but require the result, if not found shifts to Either.Left */
  def lens[B](lens: Optional[Json, B], on: Json): Either[BCJsonErr, B] = {
    lens.getOption(on).toRight(BCJsonErr(s"Lens Failed: $lens", on, None))
  }

  val rs_resultLens: Optional[Json, JsonObject] = root.result.obj
  val rs_txjsonLens: Optional[Json, JsonObject] = root.result.tx_json.obj
  val rs_txblobLens: Optional[Json, String]     = root.result.tx_blob.string
  val rs_deprecated                             = root.result.deprecated

  /** Removes the deprecated field in result iff its present*/
  def removeDeprecated(rs: Json): Json = {
    //      root.result.at("deprecated").set( None)
    root.result.obj.modify(obj => obj.remove("deprecated"))(rs.asJson)
  }

  def findResultInReply(rs: Json): Either[BCJsonErr, JsonObject] = lens(rs_resultLens, rs)
  def findTxJsonInReply(rs: Json): Either[BCJsonErr, JsonObject] = lens(rs_txjsonLens, rs)
  def findTxBlobInReply(rs: Json): Either[BCJsonErr, String]     = lens(rs_txblobLens, rs)

}
