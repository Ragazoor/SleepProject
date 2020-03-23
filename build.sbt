import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "Ragazoor-org"
ThisBuild / organizationName := "Ragazoor"

lazy val root = (project in file("."))
  .settings(
    name := "sleep_time",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.8.1",
    libraryDependencies += "org.plotly-scala" % "plotly-render_2.13" % "0.7.3"

  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
