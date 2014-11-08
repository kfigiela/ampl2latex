import sbt.Keys._
import sbt._

object WebBuild extends Build{
  lazy val parsersApi = Project(id = "mplt-web", base = file(".")).dependsOn(mplt)

  lazy val mplt = ProjectRef(id = "mplt", base = file("../"))
}