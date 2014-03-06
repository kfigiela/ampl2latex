package pl.edu.agh.mplt.parser.declaration.set

import scala.util.parsing.combinator.JavaTokenParsers

trait SetDeclarationParser extends JavaTokenParsers {
  def declaration: Parser[Any] = ???
}
