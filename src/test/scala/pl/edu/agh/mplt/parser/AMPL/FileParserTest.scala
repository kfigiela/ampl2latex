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

   def testFile(file: File): Unit = {
      it should ("parse file " + file.getName) in {

         ParsedFile.fromAMPL(file).declarations.filter {
            case InvalidDeclaration(msg) => true
            case _                       => false
         }.foreach(println)

         ParsedFile.fromAMPL(file).declarations.forall {
            case InvalidDeclaration(msg) => false
            case _                       => true
         } should not be false
      }
   }

   private def files: List[File] = resources.listFiles.filter(!_.isDirectory).toList

   private def buggedFiles: List[File] = new File("src" + / + "test"
   + / + "resources" + / + "AMPL" +
   / + "bugged").listFiles.filter(!_.isDirectory).toList

   private def localTests() = {
      try {
         val localFiles = new File("src" + / + "test"
         + / + "resources" + / + "AMPL" +
         / + "local_tests").listFiles.filter(!_.isDirectory).toList

         localFiles.foreach(testFile)
      } catch {
         case e: Throwable =>
      }
   }

   localTests()
   files.foreach(testFile)

   private def / = File.separator

   private def parse(str: String) = parser.parse(str)
}
