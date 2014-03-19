package pl.edu.agh.mplt.parser.formula.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.reference.NumberReference

trait ExpressionAMPLParser extends JavaTokenParsers {
  def arithmeticExpression: Parser[Expression]

  def number = floatingPointNumber ^^ Number

  def variable: Parser[NumberReference] = "\\w+".r ^^ NumberReference

  def expr: Parser[Expression] = arithmeticExpression | nonRecursiveExpressionProductionsParser

  def nonRecursiveExpressionProductionsParser: Parser[Expression] = List(number, variable) reduce (_ | _)

}

