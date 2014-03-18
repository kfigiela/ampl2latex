package pl.edu.agh.mplt.parser.member

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.logical.expression.Expression


trait MemberAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def stringLit: Parser[StringMember] = stringLiteral ^^ { case a => StringMember(a.drop(1).dropRight(1))}

  def member: Parser[Member] = stringLit | expr ^^ ExpressionMember
}
