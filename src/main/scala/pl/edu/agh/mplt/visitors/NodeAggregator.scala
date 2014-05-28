package pl.edu.agh.mplt.visitors

import scala.collection.mutable
import pl.edu.agh.mplt.parser.ASTNode


final class NodeAggregator[A](val mappings: mutable.Seq[NodeMapper],
                              val aggregator: Visitor[A]) {
  def apply(node: ASTNode): A =
    aggregator((mappings :\ node)((f, node) => f(node)))

}
