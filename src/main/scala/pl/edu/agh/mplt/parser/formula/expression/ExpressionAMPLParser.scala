package pl.edu.agh.mplt.parser.formula.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.reference.Reference

trait ExpressionAMPLParser extends JavaTokenParsers {
  def arithmeticExpression: Parser[Expression]

  def reference: Parser[Reference]

  def expr: Parser[Expression] = arithmeticExpression | nonRecursiveExpressionProductionsParser

  def nonRecursiveExpressionProductionsParser: Parser[Expression] =
    List(number, reference) reduce (_ | _)

  def number: Parser[Number] = floatingPointNumber ^^ Number

}

