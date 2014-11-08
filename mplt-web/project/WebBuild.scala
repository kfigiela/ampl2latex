import org.scalatra.sbt.ScalatraPlugin
import sbt.Keys._
import sbt._
import org.scalatra.sbt._
import org.scalatra.sbt.PluginKeys._
import com.mojolly.scalate.ScalatePlugin._
import ScalateKeys._

object WebBuild extends Build {
  val Name = "MPLT Web"
  val Version = "1.0.0-SNAPSHOT"
  val ScalaVersion = "2.10.2"
  val ScalatraVersion = "2.3.0"

  lazy val mplt = ProjectRef(id = "mplt", base = file("../"))

  lazy val mpltWeb = Project(
    id = "mplt-web",
    base = file("."),
    settings = ScalatraPlugin.scalatraWithJRebel ++ scalateSettings ++ Seq(
      resolvers += Classpaths.typesafeReleases,
      scalateTemplateConfig in Compile <<= (sourceDirectory in Compile) { base =>
        Seq(
          TemplateConfig(
            base / "webapp" / "WEB-INF" / "templates",
            Seq.empty, /* default imports should be added here */
            Seq(
              Binding("context", "_root_.org.scalatra.scalate.ScalatraRenderContext", importMembers = true, isImplicit = true)
            ), /* add extra bindings here */
            Some("templates")
          )
        )
      }
    )
  ).dependsOn(mplt)
}