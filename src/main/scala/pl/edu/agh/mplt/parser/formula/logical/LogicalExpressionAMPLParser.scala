package pl.edu.agh.mplt.parser.formula.logical

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.{Expression, Number}
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.reference.Reference

trait LogicalExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def member: Parser[Member]

  def sexpr: Parser[SetExpression]

  def reference: Parser[Reference]

  def lexpr: Parser[LogicalExpression] = or | nonRecursiveLogicalProductionsParser

  def keyword: Parser[String]

  def indexing: Parser[Indexing]

  private def or = chainl1(and, ("or" | "||") ^^^ Logical.or)

  private def and = chainl1(nonRecursiveLogicalProductionsParser, ("and" | "&&") ^^^ Logical.and)

  private def compareExpressions: Parser[LogicalExpression] =
    expr ~ "<" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.<(e1, e2) } |
    expr ~ "<=" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.<=(e1, e2) } |
    expr ~ ">" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.>(e1, e2) } |
    expr ~ ">=" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.>=(e1, e2) } |
    expr ~ "==" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.==(e1, e2) } |
    expr ~ "=" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.==(e1, e2) } |
    expr ~ "!=" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.!=(e1, e2) } |
    expr ~ "<>" ~ expr ^^ { case e1 ~ _ ~ e2 => Comparision.!=(e1, e2) }

  private def not: Parser[LogicalExpression] =
    ("!" | "not") ~> nonRecursiveLogicalProductionsParser ^^ { case l: LogicalExpression => Logical.not(l) }

  private def memberInclusion: Parser[LogicalExpression] =
    member ~ "in" ~ sexpr ^^ { case m ~ _ ~ (s: SetExpression) => Inclusion.member(m, s) }

  private def subsetInclusion: Parser[LogicalExpression] =
    sexpr ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ (s2: SetExpression) => Inclusion.subset(s1, s2) }

  private def memberExclusion: Parser[LogicalExpression] =
    member ~ "not" ~ "in" ~ sexpr ^^ { case m ~ _ ~ _ ~ (s: SetExpression) => Exclusion.member(m, s) }

  private def subsetExclusion: Parser[LogicalExpression] =
    sexpr ~ "not" ~ "within" ~ sexpr ^^ { case (s1: SetExpression) ~ _ ~ _ ~ (s2: SetExpression) =>
      Exclusion.subset(s1, s2)
    }

  private def reduction = keyword ~ indexing ~ lexpr ^^ {
    case "forall" ~ ind ~ lexpr => LogicalReduction.Forall(ind, lexpr)
    case "exists" ~ ind ~ lexpr => LogicalReduction.Exists(ind, lexpr)
  }

  private def parenthesized: Parser[LogicalExpression] =
    "(" ~> lexpr <~ ")" ^^ { case l: LogicalExpression => ParenthesizedLogical(l) }

  private def nonRecursiveLogicalProductionsParser: Parser[LogicalExpression] =
    List(reduction, compareExpressions, memberInclusion, memberExclusion, subsetInclusion, subsetExclusion, not,
      reference, expr ^^ (Comparision.!=(_, Number("0"))), parenthesized) reduce (_ | _)

}
