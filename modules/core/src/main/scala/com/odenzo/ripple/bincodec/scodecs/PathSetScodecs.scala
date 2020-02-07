package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data.{NonEmptyList, _}
import cats.implicits._
import io.circe.Json
import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.codecs.literals._

import com.odenzo.ripple.bincodec.scodecs.FieldScodec.xrpfield

/** These should fall under the delimited fields stuff */
trait PathSetScodecs {

  import com.odenzo.ripple.bincodec.scodecs.AmountScodecs._
  import com.odenzo.ripple.bincodec.scodecs.AccountScodecs._
  // PathSteps are fixed length based on their prefix. Will fail if applied past the end of a path step

  val pathstepAccountId: Codec[XRPLPathStep] =
    (hex"01" ~> xrpaccount)
      .xmap[XRPLPathStep](x => XRPLPathStep(hex"01".bits, account = x.some), y => y.account.get)
      .withContext("Issuer PathStep")

  val pathstepCurrency: Codec[XRPLPathStep] = (hex"10" ~> xrplCurrency)
    .xmap[XRPLPathStep](x => XRPLPathStep(setType = hex"10".bits, currency = x.some), y => y.currency.get)
    .withContext("Currency PathStep")

  val pathstepIssuer: Codec[XRPLPathStep] = (hex"20" ~> xrpaccount)
    .xmap[XRPLPathStep](x => XRPLPathStep(setType = hex"20".bits, issuer = x.some), y => y.issuer.get)
    .withContext("Issuer PathStep")

  val pathstepCurrencyIssuer: Codec[XRPLPathStep] = (hex"30" ~> xrplCurrency ~ xrpaccount)
    .xmap[XRPLPathStep](
      x => XRPLPathStep(setType = hex"30".bits, currency = x._1.some, issuer = x._2.some),
      y => (y.currency.get, y.issuer.get)
    )
    .withContext("XRP PathSetp Currency and Issuer")
  // Should peek() or get zipMap thing

  val xrplPathStep: Codec[XRPLPathStep] = choice(pathstepAccountId, pathstepCurrency, pathstepIssuer, pathstepCurrencyIssuer)

//  // Limit of 1 to 8 path steps, ordering important. The vector delimited should cut down to size before ff or 00
//  // FF and 00 can occur in a path so we go until the next "pathstep" field id is 0xFF but lets work from bottom up
//  val xrppath: Codec[XRPLPath] =
//    choice(pathstepAccountId, pathstepCurrency, pathstepIssuer, pathstepCurrencyIssuer)
//  .xmap[XRPLPath](x => XRPLPath(x), y => y.steps)
//    .withContext("XRP Path")
//
//  // Better approach to discriminate on field "field id" for PathStep and 0xff for end of path and 0x00 for end of all paths
//  val xrppathWithDelimeter: Codec[XRPLPath] = peek(byte)

  // t1:t2... ~ t(last) Limit of 1...6 paths (This is broken)
  // Quick hack. IOR with right accumulating decoded results. Left is anotherPath or endPath when "done" with a given path in pathset
  // It does not consume the Path delimeters bit it does consume the pathsteps, easy to change if needed.
  def getNextPathSetField(bv: BitVector): Either[String, DecodeResult[XRPLPathStep]] = {
    val anotherPath                   = hex"FF".bits
    val endPath                       = hex"00".bits
    val res: DecodeResult[ByteVector] = peek(bytes(1)).decode(bv).require
    val fieldId                       = res.value
    scribe.debug(s"PathField: ${fieldId.toHex} \n Remaining: ${res.remainder.toHex}")
    fieldId match {
      case endPath     => Left("end")
      case anotherPath => Left("another")
      case other       => xrplPathStep.decode(bv).require.asRight

    }
  }

  /** Slightly different approach where the State captures values and the transition result
    * signals what is coming next (if anything */
  case class PathSetState(paths: List[XRPLPath], steps: List[XRPLPathStep], remaining: BitVector) {
    def addStep(step: XRPLPathStep, left: BitVector) = this.copy(steps = step :: steps, remaining = left)

    def nextPath(left: BitVector) = {
      PathSetState(XRPLPath(steps.reverse) :: paths, List.empty, left)
    }

    def endPaths(left: BitVector) = {
      PathSetState(XRPLPath(steps.reverse) :: paths, List.empty, left)
    }
  }

  object PathSetState {
    val empty               = PathSetState(List.empty, List.empty, BitVector.empty)
    def from(bv: BitVector) = empty.copy(remaining = bv)
  }

  val stateFn: State[PathSetState, Option[BitVector]] = State[PathSetState, Option[BitVector]](state => {
    scribe.debug(s"Current State => $state")
    val fieldId = state.remaining.take(16).bytes

    fieldId match {
      case endPath     => (state.endPaths(state.remaining.drop(16)), Some(state))
      case anotherPath => (state.nextPath(state.remaining.drop(16)), None)
      case other       =>
        // Assume its a pathstep which will die if its not.
        val stepResult: DecodeResult[XRPLPathStep] = xrplPathStep.decode(state.remaining).require
        (state.addStep(stepResult.value, stepResult.remainder), None)

    }
  })

  val result: (PathSetState, Option[(Json, Json)]) = stateFn.iterateWhile(_.isEmpty).run(PathSetState.from(bv)).value

  val finalState: PathSetState = result._1

}

object PathSetScodecs extends PathSetScodecs
