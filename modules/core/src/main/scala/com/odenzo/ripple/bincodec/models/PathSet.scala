package com.odenzo.ripple.bincodec.models

import io.circe.Codec
import scodec.bits._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

case class XRPLPathStep(
    account: Option[String]        = None,
    currency: Option[XRPLCurrency] = None,
    issuer: Option[String]         = None
) {

  val code: ByteVector = (account,currency,issuer) match {
    case (Some(a), None, None) => hex"01"
    case (None, Some(c), None) => hex"10"
    case (None, None, Some(i)) => hex"20"
    case (None, Some(c), Some(i)) => hex"30"
    case _ => throw new IllegalStateException("Invalid Pathstep")
  }

}

object XRPLPathStep {
  implicit val config: Configuration = Configuration.default
  implicit val pathStepCodec: Codec.AsObject[XRPLPathStep] = deriveConfiguredCodec[XRPLPathStep]
}

case class XRPLPath(steps: List[XRPLPathStep]) {
  def append(step: XRPLPathStep): XRPLPath = XRPLPath(this.steps.appended(step))
}

object XRPLPath {
  val empty: XRPLPath = XRPLPath(List.empty[XRPLPathStep])
  implicit val config: Configuration = Configuration.default
  implicit val pathCodec: Codec.AsObject[XRPLPath]         = deriveConfiguredCodec[XRPLPath]

}

case class XRPLPathSet(paths: Vector[XRPLPath])

object XRPLPathSet {
  val empty: XRPLPathSet = XRPLPathSet(Vector.empty[XRPLPath])
  implicit val config: Configuration = Configuration.default
  implicit val pathSetCodec: Codec.AsObject[XRPLPathSet]   = deriveConfiguredCodec[XRPLPathSet]
}

