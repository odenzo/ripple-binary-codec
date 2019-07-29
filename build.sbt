import MyCompileOptions._

lazy val supportedScalaVersions = List("2.13.0", "2.12.6")
//scalaVersion := crossScalaVersions.value.head

scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, n)) if n == 12 => optsV12 ++ warningsV12 ++ lintersV12
  case Some((2, n)) if n >= 13 => optsV13 ++ warningsV13 ++ lintersV13
  case _                       => Seq("-Yno-adapted-args")
})

ThisBuild / organization := "com.odenzo"
ThisBuild / version := "0.2.6"
ThisBuild / scalaVersion := supportedScalaVersions.head
ThisBuild / name := "ripple-binary-codec"



lazy val root = (project in file("."))
  .aggregate(bincodec)
  .settings(
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
    publish / skip := true
    )



lazy val bincodec = (project in file("."))
  .settings(
    crossScalaVersions := supportedScalaVersions,
    name:= "ripple-binary-codec",
    commonSettings,
    devSettings,
    )

lazy val commonSettings = Seq(
  coverageHighlighting := true,
  libraryDependencies ++= libs,
  resolvers ++= Seq(
    Resolver.bintrayIvyRepo("odenzo", "ripple-binary-codec"),
    Resolver.defaultLocal, // Usual I pulishLocal to Ivy not maven
    Resolver.jcenterRepo // This is JFrogs Maven Repository for reading
    )
  )
val devSettings   = Seq(
  Test / logBuffered := true,
  Test / parallelExecution := false
  )
val circeVersion  = "0.12.0-M4"
val catsVersion   = "2.0.0-M4"
val spireVersion  = "0.17.0-M1"
val scribeVersion = "2.7.9"

val libs = Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsVersion,
  "org.typelevel" %% "spire" % spireVersion,
  "com.outr" %% "scribe" % scribeVersion
  )

