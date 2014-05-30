package pl.edu.agh.mplt.visitors

import scala.collection.mutable
import pl.edu.agh.mplt.parser.declaration.Declaration

final class NodeAggregator[B](val mappings: mutable.Seq[NodeMapper],
                              val aggregator: Visitor[Declaration, B]) {
   def apply(node: Declaration): B =
      aggregator((mappings :\ node)((f, node) => f.map(node)))


}
