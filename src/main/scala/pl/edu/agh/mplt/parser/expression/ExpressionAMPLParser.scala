package pl.edu.agh.mplt.parser.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.expression.variable.Variable

trait ExpressionAMPLParser extends JavaTokenParsers {
  def arithmeticExpression: Parser[Expression]

  def number = floatingPointNumber ^^ Number

  def variable: Parser[Variable] = "\\w+".r ^^ Variable

  def stringLit: Parser[StringLiteral] = stringLiteral ^^ { case a => StringLiteral(a.drop(1).dropRight(1))}

  def expr: Parser[Expression] = arithmeticExpression | nonRecursiveProductionsParser

  def nonRecursiveProductionsParser: Parser[Expression] = List(number, variable, stringLit) reduce (_ | _)

}

