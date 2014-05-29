package pl.edu.agh.mplt.visitors

import scala.collection.mutable
import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.latex.TmpVisitor

final class NodeAggregator[B](val mappings: mutable.Seq[NodeMapper],
                                            val aggregator: TmpVisitor[ASTNode, B]) {
  def apply(node: ASTNode): B =
    aggregator((mappings :\ node)((f, node) => f(node)))


}
