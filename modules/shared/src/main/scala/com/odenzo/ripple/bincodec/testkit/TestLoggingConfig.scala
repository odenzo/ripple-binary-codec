package com.odenzo.ripple.bincodec.testkit

import cats.Eval
import scribe.Level

import com.odenzo.ripple.bincodec.utils.{ByteUtils, ScribeLoggingConfig, JsonUtils}

// This should be touched once to config the Scribe logging system prior to testing.
object TestLoggingConfig {

  ScribeLoggingConfig.defaultSetup.value

  def debugLevel(): Unit     = setAll(Level.Debug)
  def setAll(l: Level): Unit = if (!ScribeLoggingConfig.inCI) ScribeLoggingConfig.setAllToLevel(l)
  def setLogToDebug(): Unit  = TestLoggingConfig.setAll(Level.Debug)
  def setLogToWarn(): Unit   = TestLoggingConfig.setAll(Level.Warn)

  val setTestLogging: Eval[Any] = Eval.always {
    if (!ScribeLoggingConfig.inCI) {
      val targetLevel = Level.Debug
      scribe.warn(s"****** Calling setAllLevel with ${targetLevel}")
      setAll(targetLevel)

      val packagesToMute: List[String] = List(
        "com.odenzo.ripple.bincodec.reference"
        //"com.odenzo.ripple.bincodec.codecs.STObjectCodec"
      )
      ScribeLoggingConfig.addModifiers(packagesToMute, Level.Warn)

      val clz: List[Class[_ >: ByteUtils with JsonUtils <: Object]] = List(classOf[ByteUtils], classOf[JsonUtils])

      // Not added as a Modifier yet
      ScribeLoggingConfig.excludeByClass(clz, Level.Debug)
    } else {
      debugLevel()
    }
  }

  def mutePackage(p: String): Unit = {
    ScribeLoggingConfig.addModifiers(List(p), Level.Warn) // TODO: This really adding, check alter
  }
}
