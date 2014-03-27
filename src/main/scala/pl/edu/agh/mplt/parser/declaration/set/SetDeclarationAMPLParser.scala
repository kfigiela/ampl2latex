package pl.edu.agh.mplt.parser.declaration.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.SetAttribute

trait SetDeclarationAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def setAttribute: Parser[SetAttribute]

  def setDeclaration: Parser[SetDeclaration] = "set" ~> string ~ (string ?) ~ (indexing ?) ~ rep(setAttribute) <~ ";" ^^ {
    case name ~ optAlias ~ optIndexing ~ optAttributes => SetDeclaration(name, optAlias, optIndexing, optAttributes)
  }

  def string = "[a-zA-Z]\\w+".r
}
