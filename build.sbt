import MyCompileOptions._

lazy val supportedScalaVersions = List("2.13.1", "2.12.9")
scalaVersion := supportedScalaVersions.head

ThisBuild / organization := "com.odenzo"
ThisBuild / name         := "ripple-binary-codec"
ThisBuild / scalaVersion := supportedScalaVersions.head

Test / logBuffered       := true
Test / parallelExecution := false

coverageMinimum       := 70
coverageFailOnMinimum := false
coverageHighlighting  := true

lazy val bincodec_root = (project in file("."))
  .aggregate(bincodec)
  .settings(
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val bincodec = (project in file("modules/core"))
  .settings(
    crossScalaVersions := supportedScalaVersions,
    name               := "ripple-binary-codec",
    scalacOptions := (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => optsV12 ++ warningsV12 ++ lintersV12
      case Some((2, n)) if n >= 13 => optsV13 ++ warningsV13 ++ lintersV13
      case _                       => Seq("-Yno-adapted-args")
    }),
    commonSettings
  )

lazy val commonSettings = Seq(
  coverageHighlighting := true,
  libraryDependencies ++= libs,
  resolvers ++= Seq(
    Resolver.defaultLocal,
    Resolver.jcenterRepo // This is JFrogs Maven Repository for reading
  )
)

val circeVersion      = "0.12.1"
val catsVersion       = "2.0.0"
val catsEffectVersion = "2.0.0"
val spireVersion      = "0.17.0-M1"
val scribeVersion     = "2.7.10"
val scalaTestVersion  = "3.0.8"
val scalaCheckVersion = "1.14.1"

val libs = Seq(
  "org.scalatest"  %% "scalatest"            % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"           % scalaCheckVersion % Test,
  "io.circe"       %% "circe-core"           % circeVersion,
  "io.circe"       %% "circe-generic"        % circeVersion,
  "io.circe"       %% "circe-generic-extras" % "0.12.2",
  "io.circe"       %% "circe-parser"         % circeVersion,
  "io.circe"       %% "circe-optics"         % "0.12.0",
  "io.circe"       %% "circe-literal"        % circeVersion % Test,
  "org.typelevel"  %% "cats-core"            % catsVersion,
  "org.typelevel"  %% "cats-effect"          % catsEffectVersion,
  "org.typelevel"  %% "spire"                % spireVersion,
  "com.outr"       %% "scribe"               % scribeVersion
)
