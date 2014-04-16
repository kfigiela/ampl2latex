package pl.edu.agh.mplt

import pl.edu.agh.mplt.parser.AMPLParser
import scala.io.Source
import pl.edu.agh.mplt.parser.declaration.Declaration
import java.io.File

class ParsedFile(val file: File, val parser: AMPLParser) {
  val ast: List[Declaration] = {
    val fileContent = Source.fromFile(file).mkString
    val res = parser.parse(fileContent)

    res match {
      case parser.Success(result: List[Declaration], _) => result
      case msg @ parser.Failure(_, _) => throw new Exception(msg.toString())
      case msg @ parser.Error(_, _) => throw new Exception(msg.toString())
    }
  }
}

object ParsedFile {
  def fromAMPL(file: File) = new ParsedFile(file, AMPLParser())
}
