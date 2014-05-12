package pl.edu.agh.mplt.parser.AMPL

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import java.io.File
import pl.edu.agh.mplt.ParsedFile
import pl.edu.agh.mplt.parser.declaration.InvalidDeclaration


class FileParserTest extends FlatSpec with Matchers with IntercodeImplicits {
  "AMPL parser" should "" in {}

  val parser = AMPLParser()

  val resources = new File("src" + / + "test" + / + "resources" + / + "AMPL")

  files.map {
    file =>
      it should ("parse file " + file.getName) in {
        //                parse(content) should  be (1)
        ParsedFile.fromAMPL(file).ast.foreach {
          case InvalidDeclaration(msg) => false //throw new Exception(msg)
          case _ => true
        }
      }
  }

  private def files: List[File] = resources.listFiles.filter(!_.isDirectory).toList

  private def buggedFiles: List[File] = new File("src" + / + "test"
    + / + "resources" + / + "AMPL" +
    / + "bugged").listFiles.filter(!_.isDirectory).toList

  private def / = File.separator

  private def parse(str: String) = parser.parseAll(parser.declarations, str)
}
