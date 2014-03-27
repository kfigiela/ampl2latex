package pl.edu.agh.mplt.parser.declaration.param

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.ParameterAttribute

trait ParameterDeclarationAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def parameterAttribute: Parser[ParameterAttribute]

  def parameterDeclaration: Parser[ParameterDeclaration] = "set" ~> string ~ (string ?) ~ (indexing ?) ~ rep(parameterAttribute) <~ ";" ^^ {
    case name ~ optAlias ~ optIndexing ~ optAttributes => ParameterDeclaration(name, optAlias, optIndexing, optAttributes)
  }

  def string = "[a-zA-Z]\\w+".r
}