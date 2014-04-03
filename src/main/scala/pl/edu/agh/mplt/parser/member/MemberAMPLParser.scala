package pl.edu.agh.mplt.parser.member

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Expression


trait MemberAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def member: Parser[Member] = singleMember | "(" ~> rep1sep(singleMember, ",") <~ ")" ^^ MultiMember

  private def singleMember: Parser[Member] = stringLit | expr ^^ ExpressionMember

  private def stringLit: Parser[StringMember] = stringLiteral ^^ {
    case a => StringMember(a.drop(1).dropRight(1))
  }
}
