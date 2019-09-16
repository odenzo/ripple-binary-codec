package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions}
import com.odenzo.ripple.bincodec.utils.ByteUtils

object BinarySerializer extends ByteUtils {

  val defns: DefinitionData = Definitions.fieldData

}
