package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data.{NonEmptyList, _}
import cats.implicits._
import com.odenzo.ripple.bincodec.models.{XRPLPath, XRPLPathSet, XRPLPathStep}
import io.circe.Json
import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.codecs.literals._
import scodec.cats._
import com.odenzo.ripple.bincodec.scodecs.FieldScodec.xrpfield
import scodec.cats._

import scala.util.Try

/** These should fall under the delimited fields stuff */
trait PathStepScodecs {

  import com.odenzo.ripple.bincodec.scodecs.AmountScodecs._
  import com.odenzo.ripple.bincodec.scodecs.AccountScodecs._
  // PathSteps are fixed length based on their prefix. Will fail if applied past the end of a path step

  val pathstepAccountId: Codec[XRPLPathStep] =
    (hex"01" ~> xrplAccount)
      .xmap[XRPLPathStep](x => XRPLPathStep(account = x.some), y => y.account.get)
      .withContext("Issuer PathStep")

  val pathstepCurrency: Codec[XRPLPathStep] = (hex"10" ~> xrplCurrency)
    .xmap[XRPLPathStep](x => XRPLPathStep(currency = x.some), y => y.currency.get)
    .withContext("Currency PathStep")

  val pathstepIssuer: Codec[XRPLPathStep] = (hex"20" ~> xrplAccount)
    .xmap[XRPLPathStep](x => XRPLPathStep(issuer = x.some), y => y.issuer.get)
    .withContext("Issuer PathStep")

  val pathstepCurrencyIssuer: Codec[XRPLPathStep] = (hex"30" ~> xrplCurrency ~ xrplAccount)
    .xmap[XRPLPathStep](
      x => XRPLPathStep(currency = x._1.some, issuer = x._2.some),
      y => (y.currency.get, y.issuer.get)
    )
    .withContext("XRP PathSetp Currency and Issuer")
  // Should peek() or get zipMap thing

  val xrplPathStep: Codec[XRPLPathStep] = choice(pathstepAccountId, pathstepCurrency, pathstepIssuer, pathstepCurrencyIssuer)

  def encode(step: XRPLPathStep): Attempt[BitVector] = {
    Attempt.fromTry {
      Try {
        val attempts: List[BitVector] = List(
          step.code.encode(()).require.some,
          step.account.map(xrplAccount.encode(_).require),
          step.currency.map(xrplCurrency.encode(_).require),
          step.issuer.map(xrplAccount.encode(_).require)
        ).flatten

        val bits = attempts.reduce(_ ++ _)
        bits
      }
    }
  }
}
object PathStepScodecs extends PathStepScodecs

trait PathSetScodecs {

  val path: Codec[List[XRPLPathStep]]     = list(PathStepScodecs.xrplPathStep) <~ constant(hex"ff")
  val pathLast: Codec[List[XRPLPathStep]] = list(PathStepScodecs.xrplPathStep) <~ constant(hex"00")

  val internalPathSet: Codec[(List[List[XRPLPathStep]], List[XRPLPathStep])] = list(path) ~ pathLast

  def encoder(pathset: XRPLPathSet): Attempt[BitVector] = {
    // First path we encode differently
    val paths    = pathset.paths.toList
    val numPaths = pathset.paths.size
    require(numPaths > 0 && numPaths < 7, s"Found $numPaths but must be 1..6 Paths in Pathset")

    val (notLast, last) = paths.splitAt(numPaths - 1)
    require(last.length === 1, "Last Path Was Empty")
    notLast.foreach { path =>
      val steps = path.steps
      val slen  = steps.length
      require(slen > 0 && slen < 9, s"Found $slen PathSteps but must be between 1...8 steps in a path")
    }

    val listed: (List[List[XRPLPathStep]], List[XRPLPathStep]) = (notLast.map(_.steps), last.head.steps)
    internalPathSet.encode(listed)

  }

  def decoder(bv: BitVector): Attempt[DecodeResult[XRPLPathSet]] = {
    Attempt.fromTry {
      Try {
        val res = PathSetState
          .stateFn
          .iterateWhile(_.isEmpty)
          .run(PathSetState.from(bv))
          .value
          ._1

        DecodeResult(XRPLPathSet(res.paths.toVector), res.remaining)
      }
    }
  }
  val xrplPathSet: Codec[XRPLPathSet] = Codec(encoder _, decoder)
}

/** Slightly different approach where the State captures values and the transition result
  * signals what is coming next (if anything */
case class PathSetState(paths: List[XRPLPath], steps: List[XRPLPathStep], remaining: BitVector) {

  // The steps list is accumulated in reverse order, and then corrected when adding to a path.
  def addStep(step: XRPLPathStep, left: BitVector): PathSetState = this.copy(steps = step :: steps, remaining = left)

  def nextPath(left: BitVector): PathSetState = {
    PathSetState(XRPLPath(steps.reverse) :: paths, List.empty, left)
  }

  // Final state where steps should be empty and the paths list ordered as found
  def endPaths(left: BitVector): PathSetState = {
    val closeSteps = nextPath(left)
    closeSteps.copy(paths = paths.reverse)
  }
}

object PathSetState {
  val empty: PathSetState               = PathSetState(List.empty, List.empty, BitVector.empty)
  def from(bv: BitVector): PathSetState = empty.copy(remaining = bv)

  private val endPath     = hex"00"
  private val anotherPath = hex"FF"

  val stateFn: State[PathSetState, Option[PathSetState]] = State[PathSetState, Option[PathSetState]](state => {
    scribe.debug(s"Current State => $state")
    val fieldId = state.remaining.take(16).bytes

    fieldId match {
      case fi if fi === endPath =>
        // Ok, we are done. We do any cleanup and present the final answer. Also need the remaining bits
        val finalState: PathSetState = state.endPaths(state.remaining.drop(16))
        (finalState, finalState.some)
      case fi if fi === anotherPath =>
        // Drop the delimieter and start accumulating a new list of pathsteps for another path
        (state.nextPath(state.remaining.drop(16)), None)
      case other =>
        // Assume its a pathstep  delimeter, which will die if its not valid. Could actually call the appropriate
        // PathStep decoder and eliminate the choice
        val stepResult: DecodeResult[XRPLPathStep] = PathStepScodecs.xrplPathStep.decode(state.remaining).require
        (state.addStep(stepResult.value, stepResult.remainder), None)

    }
  })
}
object PathSetScodecs extends PathSetScodecs
