name :=  "scala argparse"
organization := "de.halcony"
ThisBuild / description := "A re-imagined implementation of the handy python-argparse functionality for scala"
ThisBuild / versionScheme := Some("semver-spec")
Global / onChangedBuildSource := ReloadOnSourceChanges
enablePlugins(JavaAppPackaging)
lazy val scala213 = "2.13.14"
lazy val scala306 = "3.6.3"

scalaVersion := scala306

crossScalaVersions := List(scala213, scala306)

libraryDependencies ++= Seq(
  "org.scalatest"   %% "scalatest" % "3.2.19" % Test
)

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.mavenCentral,
  "Apache public" at "https://repository.apache.org/content/groups/public/"
)

scalacOptions ++= Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions",     // Allow definition of implicit functions called views
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork :=  false
testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")

checkstyleConfigLocation := CheckstyleConfigLocation.File("config/checkstyle/google_checks.xml")
checkstyleSeverityLevel := Some(CheckstyleSeverityLevel.Info)