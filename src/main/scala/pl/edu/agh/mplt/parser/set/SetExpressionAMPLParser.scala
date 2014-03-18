package pl.edu.agh.mplt.parser.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.logical.expression.Number
import pl.edu.agh.mplt.parser.logical.expression.variable.Variable
import pl.edu.agh.mplt.parser.member.Member


trait SetExpressionAMPLParser extends JavaTokenParsers {
  def member: Parser[Member]

  def number: Parser[Number]

  def variable: Parser[Variable]

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