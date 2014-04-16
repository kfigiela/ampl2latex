package pl.edu.agh.mplt.parser.AMPL

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import java.io.File
import scala.io.Source


class FileParserTest extends FlatSpec with Matchers with IntercodeImplicits {
  "AMPL parser" should "" in {}

  val parser = AMPLParser()

  val resources = new File("src" + / + "test" + / + "resources" + / + "AMPL")

  files.map {
    file =>
      it should ("parse file " + file.getName) in {
        val content = getContent(file)
        //        parse(content) should  be (1)

        parse(content) match {
          case parser.Success(_, _) =>
          case _                    => throw new Exception("File " + file.getName + " was not parsed")
        }
      }
  }

  buggedFiles.map {
    buggedFile =>
      it should ("not parse file " + buggedFile.getName) in {
        val content = getContent(buggedFile)

        parse(content) match {
          case parser.Success(_, _) => throw new Exception("File " + buggedFile.getName + " was parsed")
          case _                    =>
        }
      }
  }


  private def files: List[File] = resources.listFiles.filter(!_.isDirectory).toList

  private def buggedFiles: List[File] = new File("src" + / + "test"
                                                 + / + "resources" + / + "AMPL" +
                                                 / + "bugged").listFiles.filter(!_.isDirectory).toList

  private def / = File.separator

  private def getContent(file: File): String = Source.fromFile(file).mkString

  private def parse(str: String) = parser.parseAll(parser.declarations, str)
}
