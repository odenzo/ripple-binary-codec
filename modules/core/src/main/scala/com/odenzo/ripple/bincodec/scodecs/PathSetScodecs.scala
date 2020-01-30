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

  import com.odenzo.ripple.bincodec.scodecs.AmountScodecs._
  import com.odenzo.ripple.bincodec.scodecs.AccountScodecs._
  // PathSteps are fixed length based on their prefix

  val pathstepAccountId: Codec[XRPLPathStep] =
    (hex"01" ~> xrpaccount)
      .xmap[XRPLPathStep](x => XRPLPathStep(hex"01".bits, account = x.some), y => y.account.get)
      .withContext("Issuer PathStep")

  val pathstepCurrency: Codec[XRPLPathStep] = (hex"10" ~> xrplCurrency)
    .xmap[XRPLPathStep](x => XRPLPathStep(setType = hex"10".bits, currency = x.some), y => y.currency.get)
    .withContext("Currency PathStep")

  val pathstepIssuer: Codec[XRPLPathStep] = (hex"20" ~> xrpaccount)
    .xmap[XRPLPathStep](x => XRPLPathStep(setType = hex"10".bits, issuer = x.some), y => y.issuer.get)
    .withContext("Issuer PathStep")

  val pathstepCurrencyIssuer: Codec[XRPLPathStep] = (hex"30" ~> xrplCurrency ~ xrpaccount)
    .xmap[XRPLPathStep](
      x => XRPLPathStep(setType = hex"10".bits, currency = x._1.some, issuer = x._2.some),
      y => (y.currency.get, y.issuer.get)
    )
    .withContext("XRP PathSetp Currency and Issuer")
  // Should peek() or get zipMap thing

  // Limit of 1 to 8 path steps, ordering important. The vector delimited should cut down to size before ff or 00
  val xrppath: Codec[XRPLPath] = vector(
    choice(pathstepAccountId, pathstepCurrency, pathstepIssuer, pathstepCurrencyIssuer)
  ).xmap[XRPLPath](x => XRPLPath(x), y => y.steps)
    .withContext("XRP Path")

  // t1:t2... ~ t(last) Limit of 1...6 paths (This is broken)
  // @todo this is broken.
  val xrppathset: Codec[XRPLPathSet] =
    (vectorDelimited(hex"ff".bits, xrppath) ~ xrppath <~ constant(hex"00"))
      .xmap[XRPLPathSet](
        (x: (Vector[XRPLPath], XRPLPath)) => XRPLPathSet(x._1.appended(x._2)),
        (y: XRPLPathSet) => {
          val notLast: Vector[XRPLPath] = y.paths.take(y.paths.length - 1)
          val last: XRPLPath            = y.paths.last
          (notLast, last)
        }
      )
      .withContext("XRP PathSet")
}

object PathSetScodecs extends PathSetScodecs
