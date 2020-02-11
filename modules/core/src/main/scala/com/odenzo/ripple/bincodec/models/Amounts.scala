package com.odenzo.ripple.bincodec.models
import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration
import cats._
import cats.data._
import cats.implicits._
import cats._
import cats.data._
import cats.implicits._
import _root_.scodec.bits.BitVector
import _root_.scodec.bits._




/** Miminal Set of Models */
trait XRPLCurrency
case class CustomCurrency(custom: BitVector) extends XRPLCurrency
case class ISOCurrency(iso: String)          extends XRPLCurrency

object XRPLCurrency {
  implicit val config: Configuration = Configuration.default

  import ScodecJsonCodecs.bitVectorDecoder
  import ScodecJsonCodecs.bitVectorEncoder
  implicit val customCurrCodec: Codec[CustomCurrency] = deriveUnwrappedCodec[CustomCurrency]
  implicit val isoCurrCodec: Codec[ISOCurrency]       = deriveUnwrappedCodec[ISOCurrency]

  implicit val xrplCCurrencyEnc: Encoder[XRPLCurrency] = Encoder.instance {
    case x: CustomCurrency => x.asJson
    case x: ISOCurrency    => x.asJson
  }

  implicit val xrplCCurrencyDec: Decoder[XRPLCurrency] = Decoder[XRPLCurrency] {
    Decoder[CustomCurrency].widen or Decoder[ISOCurrency].widen
  }

}
/** This is really drops? */
trait XRPLAmount
case class XRPLDrops(amount: Long)                                                     extends XRPLAmount
case class XRPLIssuedAmount(value: BigDecimal, currency: XRPLCurrency, issuer: String) extends XRPLAmount

object XRPLAmount {


  // May re-use more general models later so but all the codecs from JSON <-> Scodec Inputs here
  implicit val config: Configuration = Configuration.default



  implicit val dropsCodec: Codec[XRPLDrops]                = deriveUnwrappedCodec[XRPLDrops]
  implicit val fiatCodec: Codec.AsObject[XRPLIssuedAmount] = deriveConfiguredCodec[XRPLIssuedAmount]

  implicit val xrplamountEnc: Encoder[XRPLAmount] = Encoder.instance {
    case x: XRPLDrops        => x.asJson
    case x: XRPLIssuedAmount => x.asJson
  }

  implicit val xplamountDec: Decoder[XRPLAmount] = Decoder[XRPLAmount] {
    Decoder[XRPLDrops].widen or Decoder[XRPLIssuedAmount].widen
  }

}
