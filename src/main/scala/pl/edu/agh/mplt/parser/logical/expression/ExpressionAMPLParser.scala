package pl.edu.agh.mplt.parser.logical.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.logical.expression.variable.Variable

trait ExpressionAMPLParser extends JavaTokenParsers {
  def arithmeticExpression: Parser[Expression]

  def number = floatingPointNumber ^^ Number

  def variable: Parser[Variable] = "\\w+".r ^^ Variable

  def expr: Parser[Expression] = arithmeticExpression | nonRecursiveExpressionProductionsParser

  def nonRecursiveExpressionProductionsParser: Parser[Expression] = List(number, variable) reduce (_ | _)

}

