package pl.edu.agh.mplt.parser.formula.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression

trait SetExpressionAMPLParser extends JavaTokenParsers {
  def member: Parser[Member]

  def number: Parser[Number]

  def reference: Parser[Reference]

  def sexpr: Parser[SetExpression] = setOperation | freeTokens

  def lexpr: Parser[LogicalExpression]

  private def ifSetExpr: Parser[SetExpression] = "if" ~> lexpr ~ "then" ~ sexpr ~ ("else" ~> sexpr) ^^ {
    case lexpr ~ _ ~ t ~ f => SetExpressionIf(lexpr, t, f)
  }

  private def setOperation = chainl1(intersection, "union" ^^^ Sets.Union | "diff" ^^^ Sets.Difference |
                                                   "symdiff" ^^^ Sets.SymetricDifference)

  private def intersection = chainl1(cross, "inter" ^^^ Sets.Intersection)

  private def cross = chainl1(freeTokens, "cross" ^^^ Sets.Cartesian)

  private def explicitSet: Parser[SetLiteral] = "{" ~> repsep(member, ",") <~ "}" ^^ {
    case members => ExplicitSet(members.toSet)
  }

  private def comprehensionSet: Parser[SetLiteral] = member ~ ".." ~ member ~ ("by" ~> number).? ^^ {
    case start ~ _ ~ end ~ byOpt => byOpt match {
      case Some(by) => SetComprehension(start, end, by)
      case _        => SetComprehension(start, end)
    }
  }

  private def parenthesized = "(" ~> sexpr <~ ")" ^^ ParenthesizedSetExpression

  private[this] def freeTokens: Parser[SetExpression] = Seq(ifSetExpr, reference, explicitSet, comprehensionSet, parenthesized)
                                                        .reduce(_ | _)

}