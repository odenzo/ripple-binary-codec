package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions}
import com.odenzo.ripple.bincodec.utils.ByteUtils

object BinarySerializer extends StrictLogging with ByteUtils {

  val defns: DefinitionData = Definitions.fieldData

  // These should be in Reference Data
  val objDel: List[UByte] = List(UByte(15))

  val arrDel: List[UByte] = List(UByte(14))


}
