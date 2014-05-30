package pl.edu.agh.mplt.visitors

import scala.collection.mutable
import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.latex.TmpVisitor
import pl.edu.agh.mplt.parser.declaration.Declaration

final class NodeAggregator[B](val mappings: mutable.Seq[NodeMapper],
                                            val aggregator: TmpVisitor[Declaration, B]) {
  def apply(node: Declaration): B =
    aggregator((mappings :\ node)((f, node) => f.map(node)))


}
