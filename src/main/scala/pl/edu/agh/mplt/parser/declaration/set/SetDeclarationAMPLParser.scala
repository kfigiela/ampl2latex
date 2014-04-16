package pl.edu.agh.mplt.parser.declaration.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.Attribute

trait SetDeclarationAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def attribute: Parser[Attribute]

  def nonKeyword: Parser[String]

  def setDeclaration: Parser[SetDeclaration] =
    "set" ~> nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ repsep(attribute, "," ?) <~ ";" ^^ {
      case name ~ optAlias ~ optIndexing ~ optAttributes => SetDeclaration(name, optAlias, optIndexing, optAttributes)
    }


}
