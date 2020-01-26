package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data.{NonEmptyList, _}
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scodec.interop.cats._
import scodec.bits._

import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.utils.JsonUtils
import cats.implicits._
import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.codecs.implicits._
import scodec.codecs.literals._

/** These should fall under the delimited fields stuff */
trait PathSetScodecs extends JsonUtils {

  import com.odenzo.ripple.bincodec.scodecs.AccountScodecs.xrpaccount
  import com.odenzo.ripple.bincodec.scodecs.AmountScodecs.xrpcurrency
  // PathSteps are fixed length based on their prefix

  val pathstepAccountId      = hex"01" ~> xrpaccount
  val pathstepCurrency       = hex"10" ~> xrpcurrency
  val pathstepIssuer         = hex"20" ~> xrpaccount
  val pathstepCurrencyIssuer = hex"30" ~> xrpcurrency ~ xrpaccount

//  val pathstep: Codec[Serializable] = choice(pathstepAccountId, pathstepCurrency, pathstepIssuer, pathstepCurrencyIssuer)
//  val path                          = pathstep // @todo  Repeat(1,6) // Now this is a list of too. Will input bitvector be fixed by pathset delimiteded? Yes.
//  // Check if VectorOf(choice) works, nope... need a repeatUntil exhausted type thing
//  // 1 to 6 delimeted paths, odd delimetering
//  val pathset = vectorDelimited(constant(hex"FF"), path) ~ path <~ constant(hex"00")
////    // TODO: Validate currency is not XRP , with special currency encoding TBC
////    // i.e. Should work with customer or psuedo-ISO (both are 160 bits)

  val xrppathset: Codec[BitVector] = fail(Err("Pathset lots of work to do"))
}

object PathSetScodecs extends PathSetScodecs
