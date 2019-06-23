package com.odenzo.ripple.bincodec

import scribe.Level.{Debug, Warn}
import scribe.{Level, Logger, Priority}
import scribe.format.FormatBlock.Level
import scribe.filter.{Filter, FilterBuilder, className, level, packageName, select}
import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.codecs.UIntCodecs

/**
  *  Scribe has run-time configuration.
  *  This is designed to control when developing the codec library and also when using.
  *  This is my experiment and learning on how to control
  *  The default config fvor scribe is INFO
  *  See com.odenzo.ripple.bincodec package information for usage.
  */
object LoggingConfig extends Logger {

  /** Helper to filter out messages in the packages given below the given level
    * I am not sure this works with the global scribe object or not.
    * Usage:
    * {{{
    *   scribe.
    * }}}
    * @return a filter that can be used with .withModifier() */
  def excludePackageSelction(packages: List[String], atOrAboveLevel: Level, priority: Priority): FilterBuilder = {
    val ps: List[Filter] = packages.map(p ⇒ packageName.startsWith(p))
    val fb               = select(ps: _*).exclude(level < atOrAboveLevel).includeUnselected.copy(priority = priority)
    fb
  }

  def excludeByClass(clazzes:List[Class[_]], minLevel: Level): FilterBuilder = {
    val names = clazzes.map(_.getName)
    scribe.info(s"Filtering Classes: $names to $minLevel")
    val filters = names.map(n⇒ className(n))
    select(filters:_* ).include(level >= minLevel)
  }



}
