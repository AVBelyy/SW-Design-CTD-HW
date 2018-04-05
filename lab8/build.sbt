name := "lab8"

version := "0.1"

scalaVersion := "2.12.4"

resolvers += "PlayRepo" at "https://github.com/playframework/playframework"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.11",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test,
  "com.typesafe.play" %% "play-json" % "2.6.9"
)

// addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.6.6")
// addSbtPlugin("play" )