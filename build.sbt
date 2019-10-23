import MyCompileOptions._

lazy val supportedScalaVersions = List("2.13.1", "2.12.10")

ThisBuild / organization := "com.odenzo"
ThisBuild / name         := "ripple-binary-codec"
ThisBuild / scalaVersion := supportedScalaVersions.head

Test / logBuffered       := true
Test / parallelExecution := false

coverageMinimum       := 70
coverageFailOnMinimum := false
coverageHighlighting  := true

lazy val bincodec_root = (project in file("."))
  .aggregate(bincodec, benchmark)
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

lazy val benchmark = (project in file("modules/benchmark"))
  .settings(
    publish            := {},
    publishLocal       := {},
    publishArtifact    := false,
    crossScalaVersions := Nil,
    javaOptions += "-XX:+UnlockCommercialFeatures"
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(bincodec) // test->compile not working in IntelliJ?

lazy val commonSettings = Seq(
  coverageHighlighting := true,
  libraryDependencies ++= libs,
  resolvers ++= Seq(
    Resolver.defaultLocal,
    Resolver.jcenterRepo // This is JFrogs Maven Repository for reading
  )
)

val circeVersion      = "0.12.3"
val catsVersion       = "2.0.0"
val catsEffectVersion = "2.0.0"
val spireVersion      = "0.17.0-M1"
val scribeVersion     = "2.7.10"
val scalaTestVersion  = "3.0.8"
val scalaCheckVersion = "1.14.2"

val libs = Seq(
  "io.circe"       %% "circe-core"           % circeVersion,
  "io.circe"       %% "circe-generic"        % circeVersion % Test,
  "io.circe"       %% "circe-generic-extras" % "0.12.2",
  "io.circe"       %% "circe-parser"         % circeVersion,
  "io.circe"       %% "circe-optics"         % "0.12.0",
  "io.circe"       %% "circe-literal"        % circeVersion % Test,
  "org.typelevel"  %% "cats-core"            % catsVersion,
  "org.typelevel"  %% "cats-effect"          % catsEffectVersion,
  "org.typelevel"  %% "spire"                % spireVersion,
  "com.outr"       %% "scribe"               % scribeVersion,
  "org.scalatest"  %% "scalatest"            % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck"           % scalaCheckVersion % Test
)
