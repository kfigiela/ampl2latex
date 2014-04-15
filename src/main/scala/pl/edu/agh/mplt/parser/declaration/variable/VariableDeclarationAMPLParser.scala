package pl.edu.agh.mplt.parser.declaration.variable

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.{PiecewiseLinearTerm, VariableAttribute}


trait VariableDeclarationAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def variableAttribute: Parser[VariableAttribute]

  def nonKeyword: Parser[String]

  def piecewiseLinearTerm :Parser[PiecewiseLinearTerm]

  def variableDeclaration: Parser[VariableDeclaration] =
    "var" ~> nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ repsep(variableAttribute, "," ?) ~ (piecewiseLinearTerm?) <~ ";" ^^ {
      case name ~ optAlias ~ optIndexing ~ optAttributes ~piecewiseOpt =>
        VariableDeclaration(name, optAlias, optIndexing, optAttributes, piecewiseOpt)
    }
}