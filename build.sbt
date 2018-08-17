name := "Scala-Stock"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2" withSources() withJavadoc(),
  "com.typesafe.akka" %% "akka-actor" % "2.5.14",
  "com.typesafe.akka" %% "akka-http"   % "10.1.3",
  "com.typesafe.akka" %% "akka-stream" % "2.5.12",
  "org.json4s" %% "json4s-native" % "3.5.3"  withSources() withJavadoc(),
  "org.json4s" %% "json4s-jackson" % "3.5.3" withSources() withJavadoc()
)