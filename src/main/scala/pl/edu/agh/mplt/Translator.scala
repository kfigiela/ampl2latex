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

  def itemize(map: mutable.LinkedHashMap[String, Stream[String]]) = map map {
    case (group, ds) =>
      val sb = new mutable.StringBuilder()
      sb.append(s"\\paragraph{$group}\n\n\n")
      sb.append("\\begin{eqnarray}\n")
      sb.append(ds.reverse.filter( _ != "" ).map(str => { s"\t${str}" }).mkString(" \\\\\n"))
      sb.append("\n\\end{eqnarray}\n\n")
      sb.toString()
  }

  def concat(map: mutable.LinkedHashMap[String, Stream[String]]) = map mapValues (_.reverse.mkString("\n"))

  def filterErrors(map: mutable.LinkedHashMap[String, Stream[String]]): mutable.LinkedHashMap[String, Stream[String]] =
    mutable.LinkedHashMap[String, Stream[String]]() ++= map.filterKeys(_ != "errors")

  def onlyFormulas(map: mutable.LinkedHashMap[String, Stream[String]]): mutable.LinkedHashMap[String, Stream[String]] =
    mutable.LinkedHashMap[String, Stream[String]]() ++= map.filterKeys(v => v != "set" && v != "param" && v != "var")


  def translate: Iterable[String] = itemize(onlyFormulas(filterErrors(ast.aggregate { case _ => latexTranslator})))
  def index: scala.collection.Map[String, String] = concat(filterErrors(ast.aggregate { case _ => referenceIndexer }))

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