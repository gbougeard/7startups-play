name := "SevenStartups"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
//  jdbc,
//  anorm,
//  cache ,
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "org.scalacheck" %% "scalacheck" % "1.11.4",
  "io.github.nicolasstucki" %% "multisets" % "0.1"
)     

play.Project.playScalaSettings
