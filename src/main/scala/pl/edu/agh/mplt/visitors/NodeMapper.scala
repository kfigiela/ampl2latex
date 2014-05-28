package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.ASTNode
import scala.collection.mutable

trait NodeMapper {
  def operations: mutable.Seq[NodeMapper]

  def apply(node: ASTNode): ASTNode

  def andThen(mapping: NodeMapper) = mapping +: operations

  def andThen[A](v: Visitor[A]): NodeAggregator[A] = new NodeAggregator[A](operations :+ this, v)
}
