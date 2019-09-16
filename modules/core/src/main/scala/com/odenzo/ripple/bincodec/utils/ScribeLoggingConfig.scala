package com.odenzo.ripple.bincodec.utils

import cats._
import cats.data._
import cats.implicits._
import scribe.{Priority, Logger, Level}
import scribe.filter._
import scribe.Level.{Warn, Debug}

/**
  *  Scribe has run-time configuration.
  *  This is designed to control when developing the codec library and also when using.
  *  This is my experiment and learning on how to control
  *  The default config fvor scribe is INFO
  *  See com.odenzo.ripple.bincodec package information for usage.
  */
object ScribeLoggingConfig extends Logger {

  /** Helper to filter out messages in the packages given below the given level
    * I am not sure this works with the global scribe object or not.
    * Usage:
    * {{{
    *   scribe.
    * }}}
    * @return a filter that can be used with .withModifier() */
  def excludePackageSelction(packages: List[String], atOrAboveLevel: Level, priority: Priority): FilterBuilder = {
    val ps: List[Filter] = packages.map(p => packageName.startsWith(p))
    val fb               = select(ps: _*).exclude(level < atOrAboveLevel).includeUnselected.copy(priority = priority)
    fb
  }

  def excludeByClass(clazzes: List[Class[_]], minLevel: Level): FilterBuilder = {
    val names = clazzes.map(_.getName)
    scribe.info(s"Filtering Classes: $names to $minLevel")
    val filters = names.map(n => className(n))
    select(filters: _*).include(level >= minLevel)
  }

  // When exactly does this get instanciated? Have to touch it.

  // Need to apply these by package scope for library mode.
  // APply to com.odenzo.ripple.bincodec.*

  scribe.warn("*********** logging config package initialization **************")

  /** Scala test should manuall control after this */
  val defaultSetup: Eval[Unit] = Eval.later {

    if (inCI) { // This should catch case when as a library in someone elses CI
      scribe.info("defaultSetup for logging IN CONTINUOUS_INTEGRATION")
      setAllToLevel(Warn)
    } else {
      setAllToLevel(Debug) // On Assumption we are in library mode, not testing, which will override.
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
    scribe.Logger.root.withModifier(ScribeLoggingConfig.excludePackageSelction(packages, l, pri)).replace()

  }

  defaultSetup.value
}