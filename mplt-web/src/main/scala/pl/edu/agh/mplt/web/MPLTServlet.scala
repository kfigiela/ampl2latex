package pl.edu.agh.mplt.web

import org.scalatra._
import pl.edu.agh.mplt.parser.AMPLParser
import scalate.ScalateSupport

class MPLTServlet extends MpltWebStack {
  get("/") {
    contentType="text/html"

    ssp("/WEB-INF/layouts/main.ssp")
  }

}
