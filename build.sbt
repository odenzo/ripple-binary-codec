import MyCompileOptions._

lazy val supportedScalaVersions = List("2.13.0", "2.12.9")
scalaVersion := supportedScalaVersions.head

ThisBuild / organization := "com.odenzo"
ThisBuild / version := "0.1.1"
ThisBuild / scalaVersion := supportedScalaVersions.head
ThisBuild / name := "ripple-binary-codec"

Test / logBuffered := true
Test / parallelExecution := false

coverageMinimum := 70
coverageFailOnMinimum := false
coverageHighlighting := true

publishArtifact in Test := false

lazy val bincodec_root = (project in file("."))
  .aggregate(bincodec)
  .settings(
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
    publish / skip := true
  )

lazy val bincodec = (project in file("modules/core"))
  .settings(
    crossScalaVersions := supportedScalaVersions,
    name := "ripple-binary-codec",
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

val circeVersion      = "0.12.0-RC4"
val catsVersion       = "2.0.0-RC3"
val catsEffectVersion = "2.0.0-RC2"
val spireVersion      = "0.17.0-M1"
val scribeVersion     = "2.7.9"
val scalaTestVersion  = "3.0.8"
val scalaCheckVersion = "1.14.0"

val libs = Seq(
  "org.scalatest"  %% "scalatest"            % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"           % scalaCheckVersion % Test,
  "io.circe"       %% "circe-core"           % circeVersion,
  "io.circe"       %% "circe-generic"        % circeVersion,
  "io.circe"       %% "circe-generic-extras" % circeVersion,
  "io.circe"       %% "circe-parser"         % circeVersion,
  "org.typelevel"  %% "cats-core"            % catsVersion,
  "org.typelevel"  %% "cats-effect"          % catsEffectVersion,
  "org.typelevel"  %% "spire"                % spireVersion,
  "com.outr"       %% "scribe"               % scribeVersion
)
