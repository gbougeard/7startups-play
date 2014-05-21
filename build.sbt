name := "SevenStartups"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache ,
  "org.scalaz" %% "scalaz-core" % "7.0.6"
)     

play.Project.playScalaSettings
