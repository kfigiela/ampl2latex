name := "mplt-web"

version := "1.0"

scalaVersion := "2.10.2"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
  "org.scalatra" %% "scalatra-json" % "2.3.0",
  "org.json4s"   %% "json4s-jackson" % "3.2.9",
  "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.1.5.v20140505" % "container",
  "org.eclipse.jetty" % "jetty-plus" % "9.1.5.v20140505" % "container",
  "javax.servlet" % "javax.servlet-api" % "3.1.0"
)
