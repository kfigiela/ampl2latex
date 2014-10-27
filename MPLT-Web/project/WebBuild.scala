import sbt._

object WebBuild extends Build{
  lazy val root = Project(id = "MPLT-Web", base = file(".")).dependsOn(mplt)

  lazy val mplt = RootProject(file("../"))
}