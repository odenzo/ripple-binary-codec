package com.odenzo.ripple.bincodec.encoding

import scala.util.Try

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.Json
import spire.math.ULong

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions}
import com.odenzo.ripple.bincodec.utils.caterrors.{BinCodecExeption, OErrorRipple, RippleCodecError}

/**
  * Better dealing with definitions data ?
  */
trait CodecUtils extends StrictLogging {

  val dd: DefinitionData = Definitions.fieldData

  /** WARNING: This doesn't check range problems */
  def bigInt2ulong(bi: BigInt): Either[OErrorRipple, ULong] = {
    if (bi < BigInt(0) || (bi > ULong.MaxValue.toBigInt))
      RippleCodecError(s"BigInt $bi out of ULong/UInt64 Range ").asLeft
    else ULong.fromBigInt(bi).asRight
  }

  /** Not sure what the Ripple spec, but we should assume all numbers are Base 10 in ""
    * for UInt64. This is as opposed to BigInt for some other numbers in quotes (e.g. Drops) */
  def parseUInt64(json: Json): Either[RippleCodecError, ULong] = {
    BinCodecExeption.wrap(s"Decoding JSON as UInt64") {

      val raw: Either[OErrorRipple, String] =
        Either.fromOption(json.asString, RippleCodecError(s"$json wasnt a string"))

      val longer: Either[RippleCodecError, ULong] = raw.flatMap { v ⇒
        Try {
          BigInt(v, 10)
        }.recover {
            case e: java.lang.NumberFormatException ⇒ BigInt(v, 16)
          }
          .toEither
          .leftMap((t: Throwable) ⇒ BinCodecExeption(t))
          .flatMap(bigInt2ulong)

      }
      longer
    }
  }
}
