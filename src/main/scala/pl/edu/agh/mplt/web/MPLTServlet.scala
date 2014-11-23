package pl.edu.agh.mplt.web

import org.scalatra._
import pl.edu.agh.mplt.Translator

import org.json4s.{DefaultFormats, Formats}

import org.scalatra.json._

class MPLTServlet extends MpltWebStack with JacksonJsonSupport{
  protected implicit val jsonFormats: Formats = DefaultFormats

  case class Response(text:String, errors: List[String])

  get("/") {
    contentType="text/html"

    ssp("/WEB-INF/layouts/main.ssp")
  }

  before(){
    contentType = formats("json")
  }

  post("/"){
    val text = params("text")
    val source = text.iterator
    val translator = Translator.fromAMPL(source)
    val result = (new StringBuilder /:translator.translate)(_.append(_).append('\n')).toString
    val errors = translator.ast.errors.toList

    Ok(body = Response(result, errors))
  }

}
