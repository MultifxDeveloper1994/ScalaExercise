// Project settings.
name := "Programming in Scala - Assignment"
organization := "co.com.psl"
version := "0.1.0"
scalaVersion := "2.12.8"
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "utf-8",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-Xlint:infer-any",
  "-Xlint:unsound-match",
  "-Ypartial-unification",
  "-Ywarn-macros:after",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
  "-Ywarn-value-discard",
  "-Dkp:genAsciiNames=true"
)

// Add the ScalaTest testing framework.
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

// Disable the sbt's streams.value.log buffering to activate the ScalaTest's built-in event buffering algorithm.
logBuffered in Test := false
