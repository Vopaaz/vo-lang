import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github"
ThisBuild / organizationName := "vopaaz"

lazy val root = (project in file("."))
  .settings(
    name := "vo-lang",
    libraryDependencies += scalaTest        % Test,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
