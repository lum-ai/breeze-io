name := "breeze-io"

version := "0.1-SNAPSHOT"

organization := "ai.lum"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "commons-io" % "commons-io" % "2.5"
)
