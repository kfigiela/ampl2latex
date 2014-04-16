package pl.edu.agh.mplt.parser.declaration.variable

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.Attribute


trait VariableDeclarationAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def attribute: Parser[Attribute]

  def nonKeyword: Parser[String]


  def variableDeclaration: Parser[VariableDeclaration] =
    "var" ~> nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ repsep(attribute, "," ?) <~ ";" ^^ {
      case name ~ optAlias ~ optIndexing ~ optAttributes =>
        VariableDeclaration(name, optAlias, optIndexing, optAttributes)
    }
}