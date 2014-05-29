package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.formula.expression.{ExpressionReduction, BinaryOperation, Expression}
import pl.edu.agh.mplt.parser.formula.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression
import pl.edu.agh.mplt.parser.declaration.constraint.{ConstraintComparison, ConstraintExpression, BoundedConstraint}
import pl.edu.agh.mplt.parser.declaration.data.Attribute
import pl.edu.agh.mplt.parser.reference.Reference


trait DummyMapper extends NodeMapper {
  override protected def apply(e: Expression): Expression = e

  override protected def apply(bin: BinaryOperation): BinaryOperation = bin

  override protected def apply(red: ExpressionReduction): ExpressionReduction = red

  override protected def apply(s: SetExpression): SetExpression = s

  override protected def apply(index: Indexing): Indexing = index

  override protected def apply(member: Member): Member = member

  override protected def apply(lexpr: LogicalExpression): LogicalExpression = lexpr

  override protected def apply(bounded: BoundedConstraint): BoundedConstraint = bounded

  override protected def apply(constraint: ConstraintExpression): ConstraintExpression = constraint

  override protected def apply(cc: ConstraintComparison): ConstraintComparison = cc

  override protected def apply(attr: Attribute): Attribute = attr

  override protected def apply(ref: Reference): Reference = ref
}
