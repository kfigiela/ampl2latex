package pl.edu.agh.mplt.visitors.latex.mappers

import pl.edu.agh.mplt.visitors.NodeMapper
import scala.collection.mutable

class NameFixer(operations: mutable.Seq[NodeMapper] = mutable.Seq()) extends NodeMapper(operations) {

  override def mapName(name: String): String = name.replace("_", "\\_")
}
