package com.odenzo.ripple.bincodec.scodecs

import cats.implicits._
import io.circe.Json
import scodec.bits._
import scodec._
import scodec.codecs._

/**
  * Essentially this is just going done the list of fields, and dealing with any prefix/suffix needed
  * e.g. end of object marker
  */
trait STObjectScodec {

  import RippleBase58Scodec._

  // @todo When encoding make sure to place objects in canonical order (prior to feeding json?)
  // Thing to test here is wether the objectEndMarker can be eagerly searched for, or can be present in the fields.
  // Not sure what variableSizeDelimited or vectorDelimited()
  // The vector may have veriable sized members. Read the source code.
  val fields: Codec[Vector[(Json, Json)]]   = vectorDelimited(FieldScodec.xrpfield)
  val objectSt: Codec[Vector[(Json, Json)]] = variableSizeDelimited(constant(RippleConstants.objectEndMarker), fields)

  val starray = variableSizeDelimited(constant(RippleConstants.arrayEndMarker), fields)
}

object STObjectScodec extends STObjectScodec
