package pl.edu.agh.mplt.parser.member

import pl.edu.agh.mplt.parser.phrase.expression.Expression
import pl.edu.agh.mplt.parser.ASTNode

trait SetMember extends ASTNode

case class ExpressionMember(expr: Expression) extends SetMember

case class StringMember(str: String) extends SetMember

case class MultiMember(members: List[SetMember]) extends SetMember
