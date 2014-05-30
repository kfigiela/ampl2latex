package pl.edu.agh.mplt

import pl.edu.agh.mplt.parser.AMPLParser
import pl.edu.agh.mplt.parser.declaration.{InvalidDeclaration, Declaration}
import java.io.File

class ParsedFile(val file: File, val parser: AMPLParser) extends Mappers {
   private def instructionStream: InstructionStream = new InstructionStream(file)

   lazy val instructions: Stream[String] = instructionStream.instructions

   lazy val ast: Stream[Declaration] = instructions.map(instruction =>
      parser.parse(instruction) match {
         case parser.Success(result: Declaration, _) => result
         case msg@parser.Failure(_, _)               =>
            InvalidDeclaration(msg.toString())
         case msg@parser.Error(_, _)                 =>
            InvalidDeclaration(msg.toString())
      }
   )

   def translate: Stream[String] = ast.map((fixParenthesis andThen fixNames andThen translateToLatex)(_))
}

object ParsedFile {
   def fromAMPL(file: File) = new ParsedFile(file, AMPLParser())
}
