import sbt.Artifact

name := "MPLT"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  // "org.slf4j" % "slf4j-log4j12" % "1.7.7",
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "runtime"
)

mainClass := Some("pl.edu.agh.mplt.App")

assemblyJarName in assembly := "mplt.jar"

mainClass in assembly := Some("pl.edu.agh.mplt.App")


test in assembly := {}

