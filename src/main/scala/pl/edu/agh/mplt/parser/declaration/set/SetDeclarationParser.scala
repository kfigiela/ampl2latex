package pl.edu.agh.mplt.parser.declaration.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.indexing.Indexing
import pl.edu.agh.mplt.parser.declaration.set.attributes.SetAttribute

trait SetDeclarationParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def attribute: Parser[SetAttribute]

  def declaration: Parser[SetDeclaration] = "set" ~> string ~ (string ?) ~ (indexing ?) ~ rep(attribute) <~ ";" ^^ {
    case name ~ optAlias ~ optIndexing ~ optAttributes => SetDeclaration(name, optAlias, optIndexing, optAttributes)
  }

  def string = "[a-zA-Z]\\w+".r
}
