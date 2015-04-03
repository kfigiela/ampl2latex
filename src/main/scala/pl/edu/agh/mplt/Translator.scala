package pl.edu.agh.mplt

import java.io.File

import pl.edu.agh.mplt.parser.AMPLParser
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.data.{ParameterDeclaration, SetDeclaration, VariableDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.parser.declaration.{Declaration, InvalidDeclaration}

import scala.collection.mutable
import scala.io.Source

class Translator(val source: Iterator[Char], val parser: AMPLParser) extends TranslatorActions {
  private def instructionStream: InstructionStream = new InstructionStream(source)

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

  def concat(map: mutable.LinkedHashMap[String, Stream[String]], separator:String = "\n") = map mapValues (_.reverse.filter(_.nonEmpty).mkString(separator))

  def filterErrors(map: mutable.LinkedHashMap[String, Stream[String]]): mutable.LinkedHashMap[String, Stream[String]] =
    mutable.LinkedHashMap[String, Stream[String]]() ++= map.filterKeys(_ != "error")

  def only(only: Set[String], map: mutable.LinkedHashMap[String, Stream[String]]): mutable.LinkedHashMap[String, Stream[String]] =
    mutable.LinkedHashMap[String, Stream[String]]() ++= map.filterKeys(only(_))


  def translate: scala.collection.Map[String, String] = concat(only(Set("objective", "constraint"), filterErrors(ast.aggregate { case _ => latexTranslator})), " \\\\\n")
  def index: scala.collection.Map[String, String] = concat(only(Set("set", "param", "var", "constraint", "objective"), filterErrors(ast.aggregate { case _ => referenceIndexer })))

  def decToStr(dec: Declaration): String = dec match {
    case SetDeclaration(_, _, _, _) => "set"
    case ParameterDeclaration(_, _, _, _) => "param"
    case VariableDeclaration(_, _, _, _) => "var"
    case ConstraintDeclaration(_, _, _, _) => "constraint"
    case _: ObjectiveDeclaration => "objective"
    case InvalidDeclaration(_) => "error"
    case Assertion(_, _) => "assertion"

    case _ => throw new Error("Unsupported Declaration")
  }

  private[this] def group(declarations: Stream[Declaration]): GroupedAST = {
    val map = mutable.LinkedHashMap[String, Stream[Declaration]]()
    for {
      dec <- declarations
      key = decToStr(dec)
      stream = map.getOrElseUpdate(key, Stream.empty)
    } map.put(key, dec #:: stream)

    new GroupedAST(map)
  }
}

object Translator {
  def fromAMPL(file: File) = {
    val source: Iterator[Char] = Source.fromFile(file).iter
    new Translator(source, AMPLParser())
  }

  def fromAMPL(source: Iterator[Char]) = new Translator(source, AMPLParser())

}