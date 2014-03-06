package pl.edu.agh.mplt.parser.expression

import scala.util.parsing.combinator.JavaTokenParsers

trait ExpressionAMPLParser extends JavaTokenParsers {
  def arithmeticExpression: Parser[Expression]

  def number = floatingPointNumber ^^ Number

  def string: Parser[StringLiteral] = stringLiteral ^^ { case a => StringLiteral(a.drop(1).dropRight(1))}

  def expr: Parser[Expression] = arithmeticExpression | nonRecursiveProductionsParser

  def nonRecursiveProductionsParser: Parser[Expression] = List(number, string) reduce (_ | _)

}

