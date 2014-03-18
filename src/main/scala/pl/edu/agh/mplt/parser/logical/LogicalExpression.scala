package pl.edu.agh.mplt.parser.logical

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.set.SetExpression
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.logical.expression.Expression


trait LogicalExpression extends ASTNode

object Logical {

  case class not(lexpr: LogicalExpression) extends LogicalExpression

  case class in(member: Member, sexpr: SetExpression) extends LogicalExpression

  case class notIn(member: Member, sexpr: SetExpression) extends LogicalExpression

  case class within(member: SetExpression, sexpr: SetExpression) extends LogicalExpression

  case class notWithin(member: SetExpression, sexpr: SetExpression) extends LogicalExpression

  case class <(l: Expression, r: Expression) extends LogicalExpression

  case class <=(l: Expression, r: Expression) extends LogicalExpression

  case class ==(l: Expression, r: Expression) extends LogicalExpression

  case class !=(l: Expression, r: Expression) extends LogicalExpression

  case class >(l: Expression, r: Expression) extends LogicalExpression

  case class >=(l: Expression, r: Expression) extends LogicalExpression

  case class or(l: LogicalExpression, r: LogicalExpression) extends LogicalExpression

  case class and(l: LogicalExpression, r: LogicalExpression) extends LogicalExpression

  case class Parenthesized(expr: LogicalExpression) extends LogicalExpression
}
