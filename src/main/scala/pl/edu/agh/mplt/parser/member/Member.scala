package pl.edu.agh.mplt.parser.member

import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.ASTNode

trait Member extends ASTNode

case class ExpressionMember(expr: Expression) extends Member

case class StringMember(str: String) extends Member

case class MultiMember(members: List[Member]) extends Member
