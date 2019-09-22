//
//addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "0.6.1")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.1")

addSbtPlugin("org.scala-js" % "sbt-scalajs"        % "1.0.0-M8") // 0.6.28 and .6.29 for my dependancioes
addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.0-M8")
//addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "0.6.23")

//addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "0.6.1")
//addSbtPlugin("org.scala-js"     % "sbt-scalajs"      % "0.6.28")
//addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.7")

//  https://github.com/scoverage/sbt-scoverage
// sbt clean coverage test
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")

// https://github.com/rtimush/sbt-updates
// List libraries that are outdates via `dependancyUpdates`
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.4.2")

// Generic Native Packaging -- Used for Docker; Packaging only, no code changes
// [[https://github.com/sbt/sbt-native-packager]]
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.25")

//
// ----------- Publishing ---------------
//

//     https://github.com/sbt/sbt-bintray
// ~/.bintray/.credentials
// bintrayWhoami
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.5")

//
// ---------- Code Coverage Goodies ---------------
//

// [[https://github.com/scoverage/sbt-scoverage]]
// sbt coverageAggregate to merge multi-module
// 1.6.0=RC2 or 1.5.1
//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

// Open Source
// https://github.com/scoverage/sbt-coveralls
// https://coveralls.io/
//addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.1.0")

// [[https://github.com/codacy/sbt-codacy-coverage]] Post test coverage to codacity
// sbt coverageAggregate after (or before) runnign all tests
// sbt clean coverage test
//sbt coverageReport
//sbt coverageAggregate
//sbt codacyCoverage
// Codacity coverage token from
//     export CODACY_PROJECT_TOKEN=%Project_Token%
addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "2.3")
//
/*
If you have a multi-module project, perform coverageAggregate as a separate command

script:
  - sbt clean coverage test coverageReport &&
    sbt coverageAggregate
after_success:
  - sbt coveralls
 */
