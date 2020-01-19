package com.odenzo.ripple.bincodec.encoding

import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.reference.DefinitionData
import com.odenzo.ripple.bincodec.reference.Definitions
import com.odenzo.ripple.bincodec.utils.JsonUtils

/**
  * Better dealing with definitions data ?
  * These are used often enough might as well optimize
  */
trait CodecUtils extends JsonUtils {

  // This is a hack for convenience, for other things that  extend this trait
  val dd: DefinitionData = Definitions.fieldData

}

object CodecUtils extends CodecUtils
