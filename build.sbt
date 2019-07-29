import MyCompileOptions._

//crossScalaVersions := Seq("2.13.0-M4-pre-20d3c21", "2.12.6")
//scalaVersion := crossScalaVersions.value.head
//scalacOptions ++=
//(CrossVersion.partialVersion(scalaVersion.value) match {
//  case Some((2, n)) if n >= 13 => Seq("-Xsource:2.14")
//  case _                       => Seq("-Yno-adapted-args")
//})
ThisBuild / organization := "com.odenzo"
ThisBuild / scalaVersion := "2.13.0"
ThisBuild / version := "0.2.5"

name := "ripple-binary-codec"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-language:postfixOps", "-language:higherKinds")

lazy val bincodec = (project in file("."))
  .settings(
    commonSettings,
    devSettings,
    scalacOptions ++= opts ++ warnings ++ linters
  )

lazy val commonSettings = Seq(
  coverageHighlighting := true,
  libraryDependencies ++= libs ++ lib_circe ++ lib_cats ++ lib_spire ++ lib_scribe,
  resolvers ++= Seq(
    Resolver.bintrayIvyRepo("odenzo", "ripple-binary-codec"),
    Resolver.defaultLocal, // Usual I pulishLocal to Ivy not maven
    Resolver.jcenterRepo // This is JFrogs Maven Repository for reading
  )
)
val devSettings = Seq(
  Test / logBuffered := true,
  Test / parallelExecution := false
)

val libs = {
  Seq(
    "org.scalatest"  %% "scalatest"  % "3.0.8"  % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  )
}

/** JSON Libs == Circe and Associated Support Libs */
val lib_circe = {
  val circeVersion = "0.12.0-M4"

  Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion
  )

}

val lib_cats = {
  val catsVersion = "2.0.0-M4"
  Seq(
    "org.typelevel" %% "cats-core"   % catsVersion,
    "org.typelevel" %% "cats-effect" % catsVersion
  )
}

val lib_spire = {
  val spireVersion = "0.17.0-M1"
  Seq(
    "org.typelevel" %% "spire" % spireVersion
  )
}

val lib_scribe = {
  Seq("com.outr" %% "scribe" % "2.7.9")
}
