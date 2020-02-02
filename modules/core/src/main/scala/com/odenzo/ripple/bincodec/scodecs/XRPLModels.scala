package com.odenzo.ripple.bincodec.scodecs

import scodec.bits.BitVector

/** Miminal Set of Models */
trait XRPLCurrency
case class CustomCurrency(custom: BitVector) extends XRPLCurrency
case class ISOCurrency(iso: String)          extends XRPLCurrency

/** This is really drops? */
trait XRPLAmount
case class XRPLDrops(amount: Long)                                                     extends XRPLAmount
case class XRPLIssuedAmount(value: BigDecimal, currency: XRPLCurrency, issuer: String) extends XRPLAmount

case class XRPLPathStep(
    setType: BitVector,
    account: Option[String] = None,
    currency: Option[XRPLCurrency] = None,
    issuer: Option[String] = None
)

case class XRPLPath(steps: Vector[XRPLPathStep])

case class XRPLPathSet(paths: Vector[XRPLPath])

object ScodecJsonCodecs {
  import io.circe._
  import io.circe.syntax._
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  import cats._
  import cats.data._
  import cats.implicits._

  // May re-use more general models later so but all the codecs from JSON <-> Scodec Inputs here
  implicit val config: Configuration = Configuration.default

  implicit val bitVectorEncoder: Encoder[BitVector] = Encoder.encodeString.contramap[BitVector](_.toHex)

  implicit val bitVectorDecoder: Decoder[BitVector] = Decoder
    .decodeString
    .emap(BitVector.fromHex(_) match {
      case None    => Left("Not Valid Hex")
      case Some(v) => Right(v)
    })
  implicit val customCurrCodec: Codec[CustomCurrency] = deriveUnwrappedCodec[CustomCurrency]
  implicit val isoCurrCodec: Codec[ISOCurrency]       = deriveUnwrappedCodec[ISOCurrency]

  implicit val xrplCCurrencyEnc: Encoder[XRPLCurrency] = Encoder.instance {
    case x: CustomCurrency => x.asJson
    case x: ISOCurrency    => x.asJson
  }

  implicit val xrplCCurrencyDec: Decoder[XRPLCurrency] = Decoder[XRPLCurrency] {
    Decoder[CustomCurrency].widen or Decoder[ISOCurrency].widen
  }

  implicit val dropsCodec: Codec[XRPLDrops]                = deriveUnwrappedCodec[XRPLDrops]
  implicit val fiatCodec: Codec.AsObject[XRPLIssuedAmount] = deriveConfiguredCodec[XRPLIssuedAmount]

  implicit val xrplamountEnc: Encoder[XRPLAmount] = Encoder.instance {
    case x: XRPLDrops        => x.asJson
    case x: XRPLIssuedAmount => x.asJson
  }

  implicit val xplamountDec: Decoder[XRPLAmount] = Decoder[XRPLAmount] {
    Decoder[XRPLDrops].widen or Decoder[XRPLIssuedAmount].widen
  }
  // These are wrong but enough for debugging for now
  implicit val pathStepCodec: Codec.AsObject[XRPLPathStep] = deriveConfiguredCodec[XRPLPathStep]
  implicit val pathCodec: Codec.AsObject[XRPLPath]         = deriveConfiguredCodec[XRPLPath]
  implicit val pathSetCodec: Codec.AsObject[XRPLPathSet]   = deriveConfiguredCodec[XRPLPathSet]
}