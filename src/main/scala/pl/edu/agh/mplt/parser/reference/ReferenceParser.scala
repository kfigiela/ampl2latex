package pl.edu.agh.mplt.parser.reference

import scala.util.parsing.combinator.JavaTokenParsers

trait ReferenceParser extends JavaTokenParsers {

  def setReference: Parser[SetReference] = reference ^^ SetReference

  def numberReference: Parser[NumberReference] = reference ^^ NumberReference

  def boolReference: Parser[BoolReference] = reference ^^ BoolReference

  private def reference: Parser[String] = "[a-zA-Z]\\w*".r
}
