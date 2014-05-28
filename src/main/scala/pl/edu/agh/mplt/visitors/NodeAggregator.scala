package pl.edu.agh.mplt.visitors

import scala.collection.mutable
import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.latex.tmp.TmpVisitor

final class NodeAggregator[A <: ASTNode, B](val mappings: mutable.Seq[NodeMapper[A]],
                                            val aggregator: TmpVisitor[A, B]) {
  def apply(node: A): B =
    aggregator((mappings :\ node)((f, node) => f(node)))


}
