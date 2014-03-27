package pl.edu.agh.mplt.parser.declaration.variable

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.VariableAttribute


trait VariableDeclarationAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def variableAttributes: Parser[VariableAttribute]

  def declaration: Parser[VariableDeclaration] = "set" ~> string ~ (string ?) ~ (indexing ?) ~ rep(variableAttributes) <~ ";" ^^ {
    case name ~ optAlias ~ optIndexing ~ optAttributes => VariableDeclaration(name, optAlias, optIndexing, optAttributes)
  }

  def string = "[a-zA-Z]\\w+".r
}