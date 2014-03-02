package pl.edu.agh.mplt.parser.expression

import scala.util.parsing.combinator.JavaTokenParsers

trait ExpressionParser extends JavaTokenParsers {
  def arithmeticExpression: Parser[Expression]

  def number: Parser[Expression]

  def expr: Parser[Expression] = arithmeticExpression | nonRecursiveProductionsParser

  def nonRecursiveProductionsParser: Parser[Expression] = List(number) reduce (_ | _)


}
