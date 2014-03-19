package pl.edu.agh.mplt.parser.formula.logical

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.{Expression, Number}
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.set.SetExpression

trait LogicalExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def member: Parser[Member]

  def sexpr: Parser[SetExpression]

  def logicalOperation: Parser[LogicalExpression] = "log" ^^ { _ => new LogicalExpression {}}

  def compareExpressions: Parser[LogicalExpression] = "compare" ^^ { _ => new LogicalExpression {}}

  def lexpr: Parser[LogicalExpression] = logicalOperation | nonRecursiveLogicalProductionsParser

  def not: Parser[LogicalExpression] =
    "not" ~> lexpr ^^ { case l: LogicalExpression => Logical.not(l)}

  def in: Parser[LogicalExpression] =
    member ~ "in" ~ sexpr ^^ { case m ~ _ ~ (s: SetExpression) => Inclusion.member(m, s)}

  def within: Parser[LogicalExpression] =
    sexpr ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ (s2: SetExpression) => Inclusion.subset(s1, s2)}

  def notIn: Parser[LogicalExpression] =
    member ~ "not" ~ "in" ~ sexpr ^^ { case m ~ _ ~ _ ~ (s: SetExpression) => Exclusion.member(m, s)}

  def notWithin: Parser[LogicalExpression] =
    sexpr ~ "not" ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ _ ~ (s2: SetExpression) => Exclusion.subset(s1, s2)}

  def parenthesized: Parser[LogicalExpression] =
    "(" ~> lexpr <~ ")" ^^ { case l: LogicalExpression => ParenthesizedLogical(l)}

  def nonRecursiveLogicalProductionsParser: Parser[LogicalExpression] =
    List(expr ^^ (Comparision.!=(_, Number("0"))), not, in, notIn, within, notWithin, parenthesized) reduce (_ | _)

}
