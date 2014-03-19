package pl.edu.agh.mplt.parser.formula.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.reference.NumberReference


trait SetExpressionAMPLParser extends JavaTokenParsers {
  def member: Parser[Member]

  def number: Parser[Number]

  def variable: Parser[NumberReference]

  def sexpr: Parser[SetExpression] = explicitSet | comprehensionSet

  private def explicitSet: Parser[SetLiteral] = emptyExplicitSet | "{" ~> rep1sep(member, ",") <~ "}" ^^ {
    case members => ExplicitSet(members.toSet)
  }

  private def emptyExplicitSet = "{" ~ "}" ^^ { case _ => ExplicitSet()}

  private def comprehensionSet: Parser[SetLiteral] = member ~ ".." ~ member ~ ("by" ~> number).? ^^ {
    case start ~ _ ~ end ~ byOpt => byOpt match {
      case Some(by) => SetComprehension(start, end, by)
      case _ => SetComprehension(start, end)
    }
  }

}