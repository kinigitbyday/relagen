import sbt._
import BuildConfig.{Dependencies, versions}

lazy val commonSettings = BuildConfig.commonSettings()

commonSettings

name := "relagen"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % versions.scalacheck % "provided"
) ++ Dependencies.testDeps

lazy val showVersion = taskKey[Unit]("Show version")
showVersion := {
  println(version.value)
}

// custom alias to hook in any other custom commands
addCommandAlias("build", "; compile")
