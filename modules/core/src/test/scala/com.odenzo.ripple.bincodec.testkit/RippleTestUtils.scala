package com.odenzo.ripple.bincodec.testkit

import cats._
import cats.data._
import cats.implicits._
import io.circe.optics.JsonPath.root
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import monocle.Optional
import scribe.Logging

case class BCError(msg: String, json: Option[Json] = None, cause: Option[Throwable] = None) extends Exception

trait RippleTestUtils extends Logging {

  /** Apply an Optional lens but require the result, if not found shifts to Either.Left */
  def lens[B](lens: Optional[Json, B], on: Json): Either[BCError, B] = {
    lens.getOption(on).toRight(BCError(s"Lens Failed: $lens", on.some))
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

  def findResultInReply(rs: Json): Either[BCError, JsonObject] = lens(rs_resultLens, rs)
  def findTxJsonInReply(rs: Json): Either[BCError, JsonObject] = lens(rs_txjsonLens, rs)
  def findTxBlobInReply(rs: Json): Either[BCError, String]     = lens(rs_txblobLens, rs)

}
