package pl.edu.agh.mplt

import java.io.File

import pl.edu.agh.mplt.parser.AMPLParser
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.data.{ParameterDeclaration, SetDeclaration, VariableDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.parser.declaration.{Declaration, InvalidDeclaration}

import scala.collection.mutable

class ParsedFile(val file: File, val parser: AMPLParser) extends Mappers {
  private def instructionStream: InstructionStream = new InstructionStream(file)

  lazy val instructions: Stream[String] = instructionStream.instructions

  def buildErrorMessage(msg: String, next: AMPLParser#Input): String =
    s"Error at column ${next.pos.column}: $msg. \n\t${next.pos.longString.replaceFirst("\n", "\n\t")}"

  lazy val declarations: Stream[Declaration] = instructions.map(
    instruction =>
      parser.parse(instruction) match {
        case parser.Success(result: Declaration, _) => result
        case parser.Failure(msg, next) => InvalidDeclaration(buildErrorMessage(msg, next))
        case parser.Error(msg, next) => InvalidDeclaration(buildErrorMessage(msg, next))
      }
  )

  lazy val ast: GroupedAST = group(declarations)

  def itemize(map: mutable.LinkedHashMap[String, Stream[String]]) = map map {
    case (str, ds) =>
      val sb = new mutable.StringBuilder()
      sb.append(s"$str: \\\\\n")
      sb.append("\\begin{itemize}\n")
      ds.foreach(str => sb.append(s"\t \\item $str \\\\\n"))
      sb.append("\\end{itemize}\n\n")
      sb.toString()
  }

  def filterErrors(map: mutable.LinkedHashMap[String, Stream[String]]): mutable.LinkedHashMap[String, Stream[String]] =
    mutable.LinkedHashMap[String, Stream[String]]() ++= map.filterKeys(_ != "errors")

  def translateVerbose: Iterable[String] = itemize(filterErrors(ast.aggregate { case _ => latexTranslator}))

  def decToStr(dec: Declaration): String = dec match {
    case SetDeclaration(_, _, _, _) => "set"
    case ParameterDeclaration(_, _, _, _) => "param"
    case VariableDeclaration(_, _, _, _) => "var"
    case ConstraintDeclaration(_, _, _, _) => "constraints"
    case _: ObjectiveDeclaration => "objectives"
    case InvalidDeclaration(_) => "errors"
    case Assertion(_, _) => "assertions"

    case _ => throw new Error("Unsupported Declaration")
  }

  private[this] def group(declarations: Stream[Declaration]): GroupedAST = {
    val map = mutable.LinkedHashMap[String, Stream[Declaration]]()
    for {
      dec <- declarations
      key = decToStr(dec)
      stream = map.getOrElseUpdate(key, Stream.empty)
    } map.put(key, dec #:: stream)

    new GroupedAST(map) //changing mutable map to immutable
  }
}

object ParsedFile {
  def fromAMPL(file: File) = new ParsedFile(file, AMPLParser())
}