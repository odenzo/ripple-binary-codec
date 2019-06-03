package com.odenzo.ripple.bincodec.serializing

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import spire.math.UInt

/**
  * Normalize the incoming transactions, erasing field = null and supplementing required fields
  * 
  */
object TxnPreprocessor {

  /** Take all the null fields out of all the JsonObjects, deep decscent */
  def stripNullFields(json: JsonObject): JsonObject = {
    val strippedFields: JsonObject = json.filter { case (field, j) ⇒ j =!= Json.Null }
    // Recurve Down
    strippedFields.mapValues { json ⇒
      json.asObject match {
        case None     ⇒ json
        case Some(jo) ⇒ stripNullFields(jo).asJson
      }
    }
  }

}

/**
*  In test fixtures these are often not filled, and the sign command filles them.
  *  So, we populate from the response.
  * @param sequence
  * @param fee
  * @param flags
  */
case class AutoFillable(sequence:UInt, fee:String, flags:UInt)


