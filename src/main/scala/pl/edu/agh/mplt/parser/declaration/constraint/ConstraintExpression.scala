package pl.edu.agh.mplt.parser.declaration.constraint

import pl.edu.agh.mplt.parser.phrase.expression.Expression
import pl.edu.agh.mplt.parser.ASTNode

sealed trait ConstraintExpression extends ASTNode

case class BoundedConstraint(leftExpression: Option[ConstraintComparison] = None,
                             expr: Expression,
                             rightExpression: Option[ConstraintComparison] = None) extends ConstraintExpression

trait ComplementaryConstraint extends ConstraintExpression

case class MixedComplementarity(expr: Expression, cexpr: BoundedConstraint) extends ComplementaryConstraint

case class SimpleComplementarity(leftBound: BoundedConstraint,
                                 rightBound: BoundedConstraint) extends ComplementaryConstraint

trait ConstraintComparison extends ASTNode

object Constraint {

   case class <=(expr: Expression) extends ConstraintComparison

   case class ==(expr: Expression) extends ConstraintComparison

   case class >=(expr: Expression) extends ConstraintComparison

}