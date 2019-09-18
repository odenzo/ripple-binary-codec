enablePlugins(JavaAppPackaging)

licenses in ThisBuild += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
homepage in ThisBuild := Some(url("https://github.com/odenzo/ripple-binary-codec"))
credentials in ThisBuild += Credentials(Path.userHome / ".sbt" / ".credentials")
credentials ++= (
  for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    username,
    password
  )
).toSeq

bintrayPackage in ThisBuild    := "ripple-binary-codec"
publishArtifact in Test        := true
publishMavenStyle in ThisBuild := true

bintrayOrganization in ThisBuild     := Some("odenzooss")
bintrayReleaseOnPublish in ThisBuild := false
bintrayPackageLabels in ThisBuild    := Seq("Ripple", "xrpl", "scala")
