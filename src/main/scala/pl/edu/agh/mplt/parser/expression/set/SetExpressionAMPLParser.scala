package pl.edu.agh.mplt.parser.expression.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.expression.{StringLiteral, Expression}

trait SetExpressionAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def sexpr: Parser[SetExpression] = explicitSet | comprehensionSet

  private def explicitSet: Parser[SetLiteral] = emptyExplicitSet | "{" ~> rep1sep(expr, ",") <~ "}" ^^ {
    case members => ExplicitSet(members.toSet)
  }

  private def emptyExplicitSet = "{" ~ "}" ^^ { case _ => ExplicitSet()}

  private def comprehensionSet: Parser[SetLiteral] = expr ~ ".." ~ expr ~ ("by" ~> expr).? ^^ {
    case start ~ _ ~ end ~ byOpt => byOpt match {
      case Some(by) => SetComprehension(start, end, by)
      case _ => SetComprehension(start, end)
    }
  }

}