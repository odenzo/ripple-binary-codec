import MyCompileOptions._

lazy val supportedScalaVersions = List("2.13.1")

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

val circeVersion              = "0.12.3"
val circeGenericExtrasVersion = "0.12.2"
val circeOpticsVersion        = "0.12.0"
val catsVersion               = "2.1.0"
val catsEffectVersion         = "2.0.0"
val spireVersion              = "0.17.0-M1"
val scribeVersion             = "2.7.10"
val scalaTestVersion          = "3.1.0"
val scalaCheckVersion         = "1.14.3"
val scodecV                   = "1.11.4"

val libs = Seq(
  "io.circe"       %% "circe-core"           % circeVersion,
  "io.circe"       %% "circe-generic"        % circeVersion,
  "io.circe"       %% "circe-generic-extras" % circeGenericExtrasVersion,
  "io.circe"       %% "circe-parser"         % circeVersion,
  "io.circe"       %% "circe-optics"         % circeOpticsVersion,
  "io.circe"       %% "circe-literal"        % circeVersion % Test,
  "org.typelevel"  %% "cats-core"            % catsVersion,
  "org.typelevel"  %% "cats-effect"          % catsEffectVersion,
  "org.typelevel"  %% "spire"                % spireVersion,
  "com.outr"       %% "scribe"               % scribeVersion,
  "org.scodec"     %% "scodec-core"          % scodecV,
  "org.scodec"     %% "scodec-bits"          % "1.1.12",
  "org.scodec"     %% "scodec-cats"          % "1.0.0",
  "org.scalatest"  %% "scalatest"            % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"           % scalaCheckVersion % Test,
  "com.lihaoyi"    %% "pprint"               % "0.5.6"
)
