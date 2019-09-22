import MyCompileOptions._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val supportedScalaVersions = List("2.13.1", "2.12.10")

ThisBuild / organization := "com.odenzo"
ThisBuild / name         := "ripple-binary-codec"
ThisBuild / scalaVersion := "2.13.1"

Test / logBuffered       := true
Test / parallelExecution := false

coverageMinimum       := 70
coverageFailOnMinimum := false
coverageHighlighting  := true

val circeVersion      = "0.12.1"
val catsVersion       = "2.0.0"
val catsEffectVersion = "2.0.0"
val spireVersion      = "0.17.0-M1"
val scribeVersion     = "2.7.10"
val scalaTestVersion  = "3.0.8"
val scalaCheckVersion = "1.14.1"
//
//lazy val bincodec_root = (project in file("."))
//  .aggregate(bincodec_shared)
//  .settings(
//    // crossScalaVersions must be set to Nil on the aggregating project
//    crossScalaVersions := Nil,
//    publish / skip     := true
//  )
//
//lazy val bincodec_shared = (project in file("modules/shared"))
//  .settings(
//    crossScalaVersions := supportedScalaVersions,
//    name               := "ripple-binary-codec",
//    scalacOptions := (CrossVersion.partialVersion(scalaVersion.value) match {
//      case Some((2, n)) if n <= 12 => optsV12 ++ warningsV12 ++ lintersV12
//      case Some((2, n)) if n >= 13 => optsV13 ++ warningsV13 ++ lintersV13
//      case _                       => Seq("-Yno-adapted-args")
//    }),
//    commonSettings
//  )
//
//lazy val commonSettings = Seq(
//  coverageHighlighting := true,
//  libraryDependencies ++= libs,
//  resolvers ++= Seq(
//    Resolver.defaultLocal,
//    Resolver.jcenterRepo // This is JFrogs Maven Repository for reading
//  )
//)
//
//val libs = Seq(
//  "org.scalatest"  %% "scalatest"            % scalaTestVersion % Test,
//  "org.scalacheck" %% "scalacheck"           % scalaCheckVersion % Test,
//  "io.circe"       %% "circe-core"           % circeVersion,
//  "io.circe"       %% "circe-generic"        % circeVersion,
//  "io.circe"       %% "circe-generic-extras" % "0.12.2",
//  "io.circe"       %% "circe-parser"         % circeVersion,
//  "io.circe"       %% "circe-optics"         % "0.12.0",
//  "io.circe"       %% "circe-literal"        % circeVersion % Test,
//  "org.typelevel"  %% "cats-core"            % catsVersion,
//  "org.typelevel"  %% "cats-effect"          % catsEffectVersion,
//  "org.typelevel"  %% "spire"                % spireVersion,
//  "com.outr"       %% "scribe"               % scribeVersion
//)

// Scribe is at ScalaJS 2.6.8
// Circe  is at ScalaJS 2.6.9
// WQell, lets trey 2.6.8 but if trouble wait until 1.0.0

lazy val bincodec_root = project
  .in(file("."))
  .aggregate(bincodec_xjs.jvm, bincodec_xjs.js)
  .settings(
    publish      := {},
    publishLocal := {}
  )
lazy val bincodec_xjs = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("modules"))
  .settings(
    name    := "foo",
    version := "0.1-SNAPSHOT",
    resolvers ++= Seq(
      Resolver.defaultLocal,
      Resolver.jcenterRepo // This is JFrogs Maven Repository for reading
    ),
    libraryDependencies ++= Seq(
      "org.scalatest"  %%% "scalatest"            % scalaTestVersion % Test,
      "org.scalacheck" %%% "scalacheck"           % scalaCheckVersion % Test,
      "io.circe"       %%% "circe-core"           % circeVersion,
      "io.circe"       %%% "circe-generic"        % circeVersion,
      "io.circe"       %%% "circe-generic-extras" % "0.12.2",
      "io.circe"       %%% "circe-parser"         % circeVersion,
      "io.circe"       %%% "circe-optics"         % "0.12.0",
      "io.circe"       %%% "circe-literal"        % circeVersion % Test,
      "org.typelevel"  %%% "cats-core"            % catsVersion,
      "org.typelevel"  %%% "cats-effect"          % catsEffectVersion,
      "org.typelevel"  %%% "spire"                % spireVersion,
      "com.outr"       %%% "scribe"               % scribeVersion
    )
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  )
  .jsSettings(
    // Add JS-specific settings here
  )
//
//lazy val fooJVM = bincodec_xjs.jvm
//lazy val fooJS  = bincodec_xjs.js
//
