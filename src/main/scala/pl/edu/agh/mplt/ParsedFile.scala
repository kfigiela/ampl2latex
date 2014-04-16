package pl.edu.agh.mplt

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.AMPLParser
import scala.io.Source

class ParsedFile(val name: String, val parser: AMPLParser) {
  val ast = {
    val fileContent = Source.fromFile(name).mkString
    parser.parse(fileContent)
  }
}

object ParsedFile {
  def fromAMPL(fileName: String) = new ParsedFile(fileName, AMPLParser())
}
