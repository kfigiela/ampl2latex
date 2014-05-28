package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.ASTNode
import scala.collection.mutable
import pl.edu.agh.mplt.visitors.latex.TmpVisitor

trait NodeMapper[A <: ASTNode] {
  def operations: mutable.Seq[NodeMapper]

  def apply(node: A): A

  def andThen(mapping: NodeMapper) = mapping +: operations

  def andThen[B](v: TmpVisitor[A, B]): NodeAggregator[A, B] = new NodeAggregator[A, B](operations :+ this, v)
}
