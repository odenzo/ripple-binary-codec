package com.odenzo.ripple.bincodec.models

import cats._
import cats.data._
import cats.implicits._
import scodec.bits.BitVector
import scodec.bits._


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

}
