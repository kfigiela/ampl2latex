package pl.edu.agh.mplt.parser.phrase.logical

import pl.edu.agh.mplt.parser.phrase.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.member.SetMember
import pl.edu.agh.mplt.parser.phrase.expression.Expression
import pl.edu.agh.mplt.parser.phrase.Phrase


trait LogicalExpression extends Phrase

case class ParenthesizedLogical(expr: LogicalExpression) extends LogicalExpression

trait Bool extends LogicalExpression

object Inclusion {

   case class Member(member: SetMember, sexpr: SetExpression) extends LogicalExpression

   case class Subset(member: SetExpression, sexpr: SetExpression) extends LogicalExpression

}

object Exclusion {

   case class Member(member: SetMember, sexpr: SetExpression) extends LogicalExpression

   case class Subset(member: SetExpression, sexpr: SetExpression) extends LogicalExpression

}

object Logical {

   case class Not(l: LogicalExpression) extends LogicalExpression

   case class Or(l: LogicalExpression, r: LogicalExpression) extends LogicalExpression

   case class And(l: LogicalExpression, r: LogicalExpression) extends LogicalExpression

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

