package pl.edu.agh.mplt.parser.formula.logical

import pl.edu.agh.mplt.parser.formula.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.Formula


sealed trait LogicalExpression extends Formula

case class ParenthesizedLogical(expr: LogicalExpression) extends LogicalExpression

trait Bool extends LogicalExpression

object Inclusion {

  case class member(member: Member, sexpr: SetExpression) extends LogicalExpression

  case class subset(member: SetExpression, sexpr: SetExpression) extends LogicalExpression

}

object Exclusion {

  case class member(member: Member, sexpr: SetExpression) extends LogicalExpression

  case class subset(member: SetExpression, sexpr: SetExpression) extends LogicalExpression

}

object Logical {

  case class not(l: LogicalExpression) extends LogicalExpression

  case class or(l: LogicalExpression, r: LogicalExpression) extends LogicalExpression

  case class and(l: LogicalExpression, r: LogicalExpression) extends LogicalExpression

}

object Comparision {

  case class <(l: Expression, r: Expression) extends LogicalExpression

  case class <=(l: Expression, r: Expression) extends LogicalExpression

  case class ==(l: Expression, r: Expression) extends LogicalExpression

  case class !=(l: Expression, r: Expression) extends LogicalExpression

  case class >(l: Expression, r: Expression) extends LogicalExpression

  case class >=(l: Expression, r: Expression) extends LogicalExpression

}

object LogicalReduction {

  case class Forall(indexing: Indexing, lexpr: LogicalExpression) extends LogicalExpression

  case class Exists(indexing: Indexing, lexpr: LogicalExpression) extends LogicalExpression

}

