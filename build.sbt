import MyCompileOptions._
import sbt.Keys.resolvers

ThisBuild / organization := "com.odenzo"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.0.1"

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
    scalacOptions ++= opts ++ warnings ++ linters,
  )


lazy val commonSettings = Seq(
  libraryDependencies ++= libs ++ lib_circe ++ lib_cats ++ lib_spire 
  resolvers ++= Seq(
    Resolver.bintrayIvyRepo("odenzo","ripple-binary-codec"),
    Resolver.defaultLocal,                      // Usual I pulishLocal to Ivy not maven
    Resolver.jcenterRepo,                       // This is JFrogs Maven Repository for reading
  )
)
val devSettings = Seq(
  Test / logBuffered := false,
  Test / parallelExecution := false,
)

/**
  * Approach to the build, which was formerly a ScalaJS and Scala cross build.
  * Have source library in Scala, with associated unit testing (ScalaTest)
  * Have an integration testing module, uses Akka/AkkaHttp and a dummy Ripple Server.
  * Integration testing scope is "it"
  *
  */
//
//import sbt.errorssummary.Plugin.autoImport._
//reporterConfig := reporterConfig.value.withColors(true)
//reporterConfig := reporterConfig.value.withShortenPaths(true)
//reporterConfig := reporterConfig.value.withColumnNumbers(true)
/** These are the base libraries used JVM
  * In addition it needs to use the library provided by rippled-utils multiproject module.
  * */
val libs = {
  Seq(
    "org.scalatest"              %% "scalatest"      % "3.0.7" % Test,
    "org.scalacheck"             %% "scalacheck"     % "1.14.0" % Test,
    "com.typesafe"               % "config"          % "1.3.4", //  https://github.com/typesafehub/config
    "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2",
    "ch.qos.logback"             % "logback-classic" % "1.2.3",
  )
}

/** JSON Libs == Circe and Associated Support Libs */
val lib_circe = {
  val circeVersion = "0.11.1"

  Seq(
    "io.circe" %% "circe-core"           % circeVersion,
    "io.circe" %% "circe-generic"        % circeVersion,
    "io.circe" %% "circe-java8"          % circeVersion,
    "io.circe" %% "circe-parser"         % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
  )

}


val lib_cats = {
  val catsVersion = "1.6.0"
  Seq(
    "org.typelevel" %% "cats-core"   % catsVersion, // Cats is pulled in via Circe for now
    "org.typelevel" %% "cats-effect" % "1.3.1" withSources () withJavadoc ()
  )
}


val lib_spire = {
  Seq(
       "org.typelevel" %% "spire" % "0.16.2",
       )
}

