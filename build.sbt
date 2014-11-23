import sbt.Artifact

name := "MPLT"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-log4j12" % "1.7.7",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
  "org.scalatra" %% "scalatra-json" % "2.3.0",
  "org.json4s" %% "json4s-jackson" % "3.2.9",
  "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.1.5.v20140505" % "compile;container",
  "org.eclipse.jetty" % "jetty-plus" % "9.1.5.v20140505" % "compile;container",
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "compile;container;provided;test" artifacts Artifact("javax.servlet", "jar", "jar")
)

