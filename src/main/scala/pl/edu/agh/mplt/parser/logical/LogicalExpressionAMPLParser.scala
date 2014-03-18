package pl.edu.agh.mplt.parser.logical

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.logical.expression.Expression
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.set.SetExpression

trait LogicalExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def logicalOperation: Parser[LogicalExpression] = "log" ^^ { _ => new LogicalExpression {}}

  def compareExpressions: Parser[LogicalExpression] = "compare" ^^ { _ => new LogicalExpression {}}

  def member: Parser[Member]

  def sexpr: Parser[SetExpression]

  def lexpr: Parser[LogicalExpression] = logicalOperation | nonRecursiveLogicalProductionsParser

  def not: Parser[LogicalExpression] = "not" ~> lexpr ^^ { case l: LogicalExpression => Logical.not(l)}

  def in: Parser[LogicalExpression] = member ~ "in" ~ sexpr ^^ { case m ~ _ ~ (s: SetExpression) => Logical.in(m, s)}

  def notIn: Parser[LogicalExpression] = member ~ "not" ~ "in" ~ sexpr ^^ { case m ~ _ ~ _ ~ (s: SetExpression) => Logical.notIn(m, s)}

  def within: Parser[LogicalExpression] = sexpr ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ (s2: SetExpression) => Logical.within(s1, s2)}

  def notWithin: Parser[LogicalExpression] = sexpr ~ "not" ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ _ ~ (s2: SetExpression) => Logical.within(s1, s2)}

  def parenthesized: Parser[LogicalExpression] = "(" ~> lexpr <~ ")" ^^ { case l: LogicalExpression => Logical.Parenthesized(l)}

  def nonRecursiveLogicalProductionsParser: Parser[LogicalExpression] = List(expr, not, in, notIn, within, notWithin, parenthesized) reduce (_ | _)

}
