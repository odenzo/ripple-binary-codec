package com.odenzo.ripple.bincodec.testkit

import cats.Eval
import scribe.Level

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.ScribeLoggingConfig

// This should be touched once to config the Scribe logging system prior to testing.
object TestLoggingConfig {

  def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false").equalsIgnoreCase("true")

  def debugLevel(): Unit     = setAll(Level.Debug)
  def setAll(l: Level): Unit = if (!inCI) ScribeLoggingConfig.setAllToLevel(l)
  def setLogToDebug(): Unit  = TestLoggingConfig.setAll(Level.Debug)
  def setLogToWarn(): Unit   = TestLoggingConfig.setAll(Level.Warn)

  val setTestLogging: Eval[Any] = Eval.always {
    if (!inCI) {
      val targetLevel = Level.Debug
      setAll(targetLevel)

      val packagesToMute: List[String] = List(
        "com.odenzo.ripple.bincodec.reference"
        //"com.odenzo.ripple.bincodec.codecs.STObjectCodec"
      )
      ScribeLoggingConfig.addModifiers(packagesToMute, Level.Trace)

      val clz: List[Class[JsonUtils]] = List(classOf[JsonUtils])

      // Not added as a Modifier yet
      ScribeLoggingConfig.excludeByClass(clz, Level.Debug)
    } else {
      debugLevel()
    }
  }

  setTestLogging.value
}
