package pl.edu.agh.mplt.parser.formula.logical

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.{Expression, Number}
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.set.SetExpression

trait LogicalExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def member: Parser[Member]

  def sexpr: Parser[SetExpression]

  def lexpr: Parser[LogicalExpression] = logicalOperation | compareExpressions | nonRecursiveLogicalProductionsParser

  private def logicalOperation: Parser[LogicalExpression] = "log" ^^ { _ => new LogicalExpression {}}

  private def compareExpressions: Parser[LogicalExpression] = "compare" ^^ { _ => new LogicalExpression {}}

  private def not: Parser[LogicalExpression] =
    "not" ~> lexpr ^^ { case l: LogicalExpression => Logical.not(l)}

  private def memberInclusion: Parser[LogicalExpression] =
    member ~ "in" ~ sexpr ^^ { case m ~ _ ~ (s: SetExpression) => Inclusion.member(m, s)}

  private def subsetInclusion: Parser[LogicalExpression] =
    sexpr ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ (s2: SetExpression) => Inclusion.subset(s1, s2)}

  private def memberExclusion: Parser[LogicalExpression] =
    member ~ "not" ~ "in" ~ sexpr ^^ { case m ~ _ ~ _ ~ (s: SetExpression) => Exclusion.member(m, s)}

  private def subsetExclusion: Parser[LogicalExpression] =
    sexpr ~ "not" ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ _ ~ (s2: SetExpression) => Exclusion.subset(s1, s2)}

  private def parenthesized: Parser[LogicalExpression] =
    "(" ~> lexpr <~ ")" ^^ { case l: LogicalExpression => ParenthesizedLogical(l)}

  private def nonRecursiveLogicalProductionsParser: Parser[LogicalExpression] =
    List(expr ^^ (Comparision.!=(_, Number("0"))), not, memberInclusion, memberExclusion, subsetInclusion, subsetExclusion, parenthesized) reduce (_ | _)

}
