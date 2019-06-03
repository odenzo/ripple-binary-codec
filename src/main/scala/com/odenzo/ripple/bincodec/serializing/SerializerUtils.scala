package com.odenzo.ripple.bincodec.serializing

import scala.util.Try

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import io.circe.Json
import spire.math.ULong

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions}
import com.odenzo.ripple.bincodec.serializing.BinarySerializer.FieldData
import com.odenzo.ripple.bincodec.utils.caterrors.{AppError, AppException, OError}

/**
 * Better dealing with definitions data ?
  */
trait SerializerUtils extends StrictLogging {

  val dd: DefinitionData = Definitions.fieldData

  def singleFieldData(fieldName: String, fieldValue: Json): Either[AppError, FieldData] = {
    dd.getFieldInfo(fieldName).flatMap { fi ⇒
      FieldData(fieldName, fieldValue, fi).asRight
    }
  }


  /** WARNING: This doesn't check range problems */
  def bigInt2ulong(bi: BigInt): Either[OError, ULong] = {
    if (bi < BigInt(0) || (bi > ULong.MaxValue.toBigInt)) AppError(s"BigInt $bi out of ULong/UInt64 Range ").asLeft
    else ULong.fromBigInt(bi).asRight
  }

  def parseUInt64(json: Json): Either[AppError, ULong] = {
    AppException.wrap(s"Decoding JSON as UInt64"){

      val raw: Either[OError, String] = Either.fromOption(json.asString, AppError(s"$json wasnt a string"))

      val longer: Either[AppError, ULong] = raw.flatMap{ v ⇒
        Try{
          BigInt(v, 10)
        }.recover{
          case e: java.lang.NumberFormatException ⇒ BigInt(v, 16)
        }
        .toEither
        .leftMap((t: Throwable) ⇒ AppException(t))
        .flatMap(bigInt2ulong)

      }
      longer
    }
  }
}
