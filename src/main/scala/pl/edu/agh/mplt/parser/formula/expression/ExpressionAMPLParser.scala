package pl.edu.agh.mplt.parser.formula.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.reference.NumberReference

trait ExpressionAMPLParser extends JavaTokenParsers {
  def arithmeticExpression: Parser[Expression]

  def numberReference: Parser[NumberReference]

  def expr: Parser[Expression] = arithmeticExpression | nonRecursiveExpressionProductionsParser

  def nonRecursiveExpressionProductionsParser: Parser[Expression] = List(number, numberReference) reduce (_ | _)

  def number: Parser[Number] = floatingPointNumber ^^ Number

}

