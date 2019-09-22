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

  def inCI: Boolean = scala.sys.env.getOrElse("CONTINUOUS_INTEGRATION", "false") === "true"

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
    val names   = clazzes.map(_.getName)
    val filters = names.map(n => className(n))
    select(filters: _*).include(level >= minLevel)
  }

  val defaultSetup: Eval[Unit] = Eval.later {

    if (inCI) { // This should catch case when as a library in someone elses CI
      scribe.info("defaultSetup for logging IN CONTINUOUS_INTEGRATION")
      setAllToLevel(Warn)
    } else {
      setAllToLevel(Debug) // On Assumption we are in library mode, not testing, which will override.
    }
    scribe.info("Done with Default")
  }

  def setAllToLevel(l: Level): Unit = {
    scribe.Logger.root.clearHandlers().withHandler(minimumLevel = Some(l)).replace()
    //scribe.Logger.root.clearModifiers().withMinimumLevel(l).replace()
  }

  def addModifiers(packages: List[String], l: Level): Unit = {
    val pri = Priority.Normal // unnecessary since clearing existing modifiers, but handy for future.
    scribe.Logger.root.withModifier(ScribeLoggingConfig.excludePackageSelction(packages, l, pri)).replace()

  }

  defaultSetup.value
}
