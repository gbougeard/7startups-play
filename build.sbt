import Dependencies._

name := "SevenStartups"

version := "1.0-SNAPSHOT"

//scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
//  play.Project.jdbc,
//  play.Project.cache,
//  play.Project.filters,
//  ws,
  webjars,
  wjBootstrap,
  wjFontAwesome,
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "org.scalacheck" %% "scalacheck" % "1.11.4",
  "io.github.nicolasstucki" %% "multisets" % "0.1"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)