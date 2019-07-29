package com.odenzo.ripple

import cats._
import cats.data._
import cats.implicits._
import scribe.{Level, Logger, Priority}
import scribe.Level.{Debug, Info, Trace, Warn}

package object bincodec {

  // When exactly does this get instanciated? Have to touch it.

  // Need to apply these by package scope for library mode.
  // APply to com.odenzo.ripple.bincodec.*

  scribe.warn("*********** bincodec package initialization **************")

  /** Scala test should manuall control after this */
  val defaultSetup: Eval[Unit] = Eval.later {

    if (inCI) { // This should catch case when as a library in someone elses CI
      scribe.info("defaultSetup for logging IN CONTINUOUS_INTEGRATION")
      setAllToLevel(Warn)
    } else {
      setAllToLevel(Warn) // On Assumption we are in library mode, not testing, which will override.
    }
    scribe.info("Done with Default")
  }

  /** This sets the handler filter level,  all settings to modifiers are essentially overridden on level,
    * althought the modifiers may filter out additional things.
    *
    * */
  def setAllToLevel(l: Level): Unit = {
    scribe.warn(s"Setting all to log level $l")
    scribe.Logger.root.clearHandlers().withHandler(minimumLevel = Some(l)).replace()
    //scribe.Logger.root.clearModifiers().withMinimumLevel(l).replace()
  }

  def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") === "true"

  // Well, as far as I can tell the flow is: Logger => Modifiers => Handlers, The handlers write with Formatters
  // but the default console handler (at least) can also filter with minimumLogLevel
  // This experiment has scribe.Logger.root set at DEBUG.
  // We want to filter the debug messages just for com.odenzo.ripple.bincodec.reference.FieldInfo
  // method encodeFieldID but do just for package s
  def addModifiers(packages: List[String], l: Level): Unit = {
    scribe.info(s"Setting Packages Level to $l")
    val pri = Priority.Normal // unnecessary since clearing existing modifiers, but handy for future.
    scribe.Logger.root.withModifier(LoggingConfig.excludePackageSelction(packages, l, pri)).replace()

  }

}
