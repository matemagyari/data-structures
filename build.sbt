import Dependencies._

ThisBuild / name := "data-structures"
ThisBuild / organization := "home.datastructures"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1-SNAPSHOT"

ThisBuild / libraryDependencies ++= testLibs

ThisBuild / resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Default" at "https://repo1.maven.org/maven2"
)
