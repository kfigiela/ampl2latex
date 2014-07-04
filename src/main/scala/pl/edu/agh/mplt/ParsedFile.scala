package pl.edu.agh.mplt

import pl.edu.agh.mplt.parser.{ASTNode, AMPLParser}
import pl.edu.agh.mplt.parser.declaration.{InvalidDeclaration, Declaration}
import java.io.File
import pl.edu.agh.mplt.parser.declaration.data.{VariableDeclaration, ParameterDeclaration, SetDeclaration}
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import scala.collection.mutable

class ParsedFile(val file: File, val parser: AMPLParser) extends Mappers {
  private def instructionStream: InstructionStream = new InstructionStream(file)

  lazy val instructions: Stream[String] = instructionStream.instructions

  lazy val declarations: Stream[Declaration] = instructions.map(instruction =>
    parser.parse(instruction) match {
      case parser.Success(result: Declaration, _) => result
      case msg@parser.Failure(_, _) => InvalidDeclaration(msg.toString())
      case msg@parser.Error(_, _) => InvalidDeclaration(msg.toString())
    }
  )

  lazy val ast: GroupedAST = group(declarations) {
    case SetDeclaration(_, _, _, _) => "set"
    case ParameterDeclaration(_, _, _, _) => "param"
    case VariableDeclaration(_, _, _, _) => "var"
    case ConstraintDeclaration(_, _, _, _) => "constraints"
    case _: ObjectiveDeclaration => "objectives"
    case InvalidDeclaration(_) => "errors"
    case Assertion(_, _) => "assertions"

    case _ => throw new Error("Unsupported Declaration")
  }

  def translateVerbose: mutable.LinkedHashMap[String, Stream[String]] = ast.aggregate {
    case _ => latexTranslator
  }

  private[this] def group(declarations: Stream[Declaration])(f: ASTNode => String): GroupedAST = {
    val map = mutable.LinkedHashMap[String, Stream[Declaration]]("set" -> Stream.empty, "param" -> Stream.empty,
      "var" -> Stream.empty, "objectives" -> Stream.empty, "constraints" -> Stream.empty)
    for {
      dec <- declarations
      key = f(dec)
      stream = map.getOrElseUpdate(key, Stream.empty)
    } map.put(key, dec #:: stream)

    new GroupedAST(map) //changing mutable map to immutable
  }
}

object ParsedFile {
  def fromAMPL(file: File) = new ParsedFile(file, AMPLParser())
}
