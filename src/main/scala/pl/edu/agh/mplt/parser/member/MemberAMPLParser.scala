package pl.edu.agh.mplt.parser.member

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.member.Member


trait MemberAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  private def stringLit: Parser[StringMember] = stringLiteral ^^ { case a => StringMember(a.drop(1).dropRight(1))}

  private def singleMember: Parser[Member] = stringLit | expr ^^ ExpressionMember

  def member: Parser[Member] = singleMember | "(" ~> rep1sep(singleMember, ",") ^^ MultiMember
}
