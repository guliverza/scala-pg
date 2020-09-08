scalaVersion := "2.12.12"
version := "0.1.0-SNAPSHOT"
organization := "com.guliverza"
organizationName := "example"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5",
  "org.typelevel" %% "cats-core" % "2.0.0",
)

scalacOptions += "-Ypartial-unification"

logLevel := Level.Debug
