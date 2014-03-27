package pl.edu.agh.mplt.parser.declaration.variable

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.VariableAttribute


trait VariableDeclarationAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def variableAttribute: Parser[VariableAttribute]

  def nonKeyword: Parser[String]

  def variableDeclaration: Parser[VariableDeclaration] = "var" ~> nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ rep(variableAttribute) <~ ";" ^^ {
    case name ~ optAlias ~ optIndexing ~ optAttributes => VariableDeclaration(name, optAlias, optIndexing, optAttributes)
  }
}