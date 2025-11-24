ThisBuild / organization := "de.halcony"
ThisBuild / organizationName := "halcony"
ThisBuild / organizationHomepage := Some(url("https://github.com/simkoc/"))

// this is required for sonatype sync requirements
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/simkoc/scala-argparse"),
    "scm:git@github.com:simkoc/scala-argparse.git"
  )
)

// this is required for sonatype sync requirements
ThisBuild / developers := List(
  Developer(
    id   = "simkoc",
    name = "Simon Koch",
    email = "ossrh@halcony.de",
    url = url("https://github.com/simkoc/")
  )
)

ThisBuild / description := "A simple action based command line argument parser for scala"
// this is required for sonatype sync requirements
ThisBuild / licenses := List("MIT" -> url("https://opensource.org/license/mit"))
// this is required for sonatype sync requirements
ThisBuild / homepage := Some(url("https://github.com/simkoc/scala-argparse"))
ThisBuild / versionScheme := Some("semver-spec")
// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true
// new setting for the Central Portal
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}