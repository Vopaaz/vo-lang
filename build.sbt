import Dependencies._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.0.1"
ThisBuild / organization := "com.github"
ThisBuild / organizationName := "vopaaz"

parallelExecution in Test := false

lazy val root = (project in file("."))
  .settings(
    name := "vo-lang",
    libraryDependencies += scalaTest        % Test,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
