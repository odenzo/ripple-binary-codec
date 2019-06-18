import MyCompileOptions._

//crossScalaVersions := Seq("2.13.0-M4-pre-20d3c21", "2.12.6")
//scalaVersion := crossScalaVersions.value.head
//scalacOptions ++=
//(CrossVersion.partialVersion(scalaVersion.value) match {
//  case Some((2, n)) if n >= 13 => Seq("-Xsource:2.14")
//  case _                       => Seq("-Yno-adapted-args")
//})
ThisBuild / organization := "com.odenzo"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1-snaphot"

name := "ripple-binary-codec"

scalacOptions ++= Seq("-feature",
                      "-deprecation",
                      "-unchecked",
                      "-language:postfixOps",
                      "-language:higherKinds",
                      "-Ypartial-unification")

lazy val bincodec = (project in file("."))
  .settings(
    commonSettings,
    devSettings,
    scalacOptions ++= opts ++ warnings ++ linters
  )

lazy val commonSettings = Seq(
  libraryDependencies ++= libs ++ lib_circe ++ lib_cats ++ lib_spire,
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
    "org.scalatest"              %% "scalatest"      % "3.0.7" % Test,
    "org.scalacheck"             %% "scalacheck"     % "1.14.0" % Test,

    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2", // Java Only
    "ch.qos.logback"             % "logback-classic" % "1.2.3"  // Java Only
  )
}

/** JSON Libs == Circe and Associated Support Libs */
val lib_circe = {
  val circeVersion = "0.11.1"

  Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-java8"   % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion
  )

}

val lib_cats = {
  val catsVersion = "1.6.1"
  Seq(
    "org.typelevel" %% "cats-core"   % catsVersion, 
    "org.typelevel" %% "cats-effect" % "1.3.1" 
  )
}

val lib_spire = {
  Seq(
    "org.typelevel" %% "spire" % "0.16.2"
  )
}
