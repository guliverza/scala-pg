scalaVersion := "2.13.3"
version := "0.1.0-SNAPSHOT"
organization := "com.guliverza"
organizationName := "example"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.0",
  "org.typelevel" %% "cats-core" % "2.0.0",
)

logLevel := Level.Debug
