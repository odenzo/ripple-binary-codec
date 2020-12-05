import MyCompileOptions._

lazy val supportedScalaVersions = List("2.13.3")

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

val circeVersion              = "0.13.0"
val circeGenericExtrasVersion = "0.13.0"
val circeOpticsVersion        = "0.13.0"
val catsVersion               = "2.3.0"
val catsEffectVersion         = "2.3.0"
val spireVersion              = "0.17.0"
val scribeVersion             = "3.1.7"
val scalaTestVersion          = "3.2.3"
val scalaCheckVersion         = "1.15.1"
val scodecV                   = "1.11.7"

val libs = Seq(
  "io.circe" %% "circe-core"           % circeVersion,
  "io.circe" %% "circe-generic"        % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeGenericExtrasVersion,
  "io.circe" %% "circe-parser"         % circeVersion,
  "io.circe" %% "circe-optics"         % circeOpticsVersion,
  "io.circe" %% "circe-literal"        % circeVersion,
  "io.circe" %% "circe-scodec"         % circeVersion,
  //  "io.circe"       %% "circe-spire"          % "0.1.0",   Meh, stuck at 2.12
  "org.typelevel"  %% "cats-core"   % catsVersion,
  "org.typelevel"  %% "cats-effect" % catsEffectVersion,
  "org.typelevel"  %% "spire"       % spireVersion,
  "com.outr"       %% "scribe"      % scribeVersion,
  "org.scodec"     %% "scodec-core" % scodecV,
  "org.scodec"     %% "scodec-bits" % "1.1.22",
  "org.scodec"     %% "scodec-cats" % "1.0.0",
  "org.scalatest"  %% "scalatest"   % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"  % scalaCheckVersion % Test,
  "com.lihaoyi"    %% "pprint"      % "0.6.0"
)
