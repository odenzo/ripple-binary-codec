package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data.{NonEmptyList, _}

import com.odenzo.ripple.bincodec.utils.JsonUtils
import cats.implicits._
import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.codecs.literals._

/** These should fall under the delimited fields stuff */
trait PathSetScodecs extends JsonUtils {

  import com.odenzo.ripple.bincodec.scodecs.AccountScodecs.xrpaccount
  import com.odenzo.ripple.bincodec.scodecs.AmountScodecs.xrplCurrency
  // PathSteps are fixed length based on their prefix

  val pathstepAccountId      = hex"01" ~ xrpaccount
  val pathstepCurrency       = hex"10" ~ xrplCurrency
  val pathstepIssuer         = hex"20" ~ xrpaccount
  val pathstepCurrencyIssuer = hex"30" ~ xrplCurrency ~ xrpaccount

  /// Need to return the pathset with type of pathstep too. The hex code or decimal, both forms included in JSON
  val xrppathset: Codec[BitVector] = fail(Err("Pathset lots of work to do"))
}

object PathSetScodecs extends PathSetScodecs
