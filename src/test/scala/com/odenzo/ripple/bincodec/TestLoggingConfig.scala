package com.odenzo.ripple.bincodec

import scribe.Level

import com.odenzo.ripple.bincodec
import com.odenzo.ripple.bincodec.encoding.TypeSerializers
import com.odenzo.ripple.bincodec.utils.{ByteUtils, JsonUtils}


// This should be touched once to config the Scribe logging system prior to testing.
object TestLoggingConfig {

  com.odenzo.ripple.bincodec.defaultSetup
  
  val x = setTestLogging

  lazy val setTestLogging: Unit = {
    if (!bincodec.inCI) {
      scribe.warn(s"****** Calling setAllLevel with ${Level.Debug}")
      bincodec.setAllToLevel(Level.Debug)
//      scribe.debug(s"DEBUG is on")
//      scribe.info("INFO is on")
//      scribe.warn("WARN is on")
//      scribe.error("ERROR is on")
//      scribe.warn("Should be setting packages of interest here.")

      // This don't cut out by class or method.
      val packagesToMute: List[String] = List(
                                               "com.odenzo.ripple.bincodec.reference",
                                               "com.odenzo.ripple.bincodec.codecs.STObjectCodec",

                                               )
      bincodec.replaceModifiers(packagesToMute, Level.Warn)

      val clz: List[Class[_ >: ByteUtils with JsonUtils <: Object]] = List(classOf[ByteUtils], classOf[JsonUtils])

      // Not added as a Modifier yet
      bincodec.LoggingConfig.excludeByClass(clz,Level.Debug)
    }
  }
}
