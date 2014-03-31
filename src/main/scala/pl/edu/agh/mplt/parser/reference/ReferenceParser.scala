package pl.edu.agh.mplt.parser.reference

import scala.util.parsing.combinator.JavaTokenParsers

trait ReferenceParser extends JavaTokenParsers {

  def nonKeyword: Parser[String]

  def setReference: Parser[SetReference] = nonKeyword ^^ SetReference

  def numberReference: Parser[NumberReference] = nonKeyword ^^ NumberReference

  def boolReference: Parser[BoolReference] = nonKeyword ^^ BoolReference

}
