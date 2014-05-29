package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.ASTNode
import scala.collection.mutable
import pl.edu.agh.mplt.visitors.latex.TmpVisitor
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.parser.declaration.data.Attribute
import pl.edu.agh.mplt.parser.declaration.constraint.{BoundedConstraint, ConstraintExpression, ConstraintComparison}
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.set.{SetExpression, Indexing}
import pl.edu.agh.mplt.parser.formula.expression.{Expression, BinaryOperation, ExpressionReduction}

trait NodeMapper {
  val operations: mutable.Seq[NodeMapper]

  def apply(node: ASTNode): ASTNode

  def andThen(mapping: NodeMapper): NodeMapper = {
    mapping +: operations
    this
  }

  def andThen[B](v: TmpVisitor[ASTNode, B]): NodeAggregator[B] = new NodeAggregator[B](operations :+ this, v)


  protected def apply(e: Expression): Expression

  protected def apply(bin: BinaryOperation): BinaryOperation

  protected def apply(bin: ExpressionReduction): ExpressionReduction

  protected def apply(s: SetExpression): SetExpression

  protected def apply(index: Indexing): Indexing

  protected def apply(member: Member): Member

  protected def apply(l: LogicalExpression): LogicalExpression

  protected def apply(bounded: BoundedConstraint): BoundedConstraint

  protected def apply(constraint: ConstraintExpression): ConstraintExpression

  protected def apply(cc: ConstraintComparison): ConstraintComparison

  protected def apply(attr: Attribute): Attribute

  protected def apply(ref: Reference): Reference
}
