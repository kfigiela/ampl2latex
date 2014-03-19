package pl.edu.agh.mplt.parser.formula.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.reference.{SetReference, NumberReference}


trait SetExpressionAMPLParser extends JavaTokenParsers {
  def member: Parser[Member]

  def number: Parser[Number]

  def setReference: Parser[SetReference]

  def sexpr: Parser[SetExpression] = setReference | explicitSet | comprehensionSet

  private def explicitSet: Parser[SetLiteral] = "{" ~> repsep(member, ",") <~ "}" ^^ {
    case members => ExplicitSet(members.toSet)
  }

  private def comprehensionSet: Parser[SetLiteral] = member ~ ".." ~ member ~ ("by" ~> number).? ^^ {
    case start ~ _ ~ end ~ byOpt => byOpt match {
      case Some(by) => SetComprehension(start, end, by)
      case _ => SetComprehension(start, end)
    }
  }

}