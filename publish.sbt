
//
//bintrayRepository := (if (isSnapshot.value) "sbt-plugin-snapshots" else "sbt-plugins")
//
//bintrayOrganization in bintray := None
//
//bintrayReleaseOnPublish := isSnapshot.value
enablePlugins(JavaAppPackaging)

/*
 publish and publishLocal Tasks are reasonable for developmnet.

 publishTo can go to Maven and there is a seperate JFrog/Bintray plugin to publish too


 I would like to disable Scaladoc in the publishLocal.
 */


licenses := List( ("BSD", url("https://www.apache.org/licenses/LICENSE-2.0")))
homepage := Some(url("https://github.com/odenzo/ripple-binary-codec"))
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")



//publishArtifact in Test := true // to add the tests JAR
publishArtifact in Test := false
publishMavenStyle := false
bintrayOrganization := Some("odenzoorg")
bintrayRepository := "odenzooss"
bintrayPackage := "ripple-binary-codec"
bintrayReleaseOnPublish in ThisBuild := false
