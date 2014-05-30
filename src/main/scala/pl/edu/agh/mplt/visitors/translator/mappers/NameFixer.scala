package pl.edu.agh.mplt.visitors.translator.mappers

import pl.edu.agh.mplt.visitors.NodeMapper
import scala.collection.mutable

class NameFixer(operations: mutable.Buffer[NodeMapper] = mutable.Buffer()) extends NodeMapper(operations) {

   override def mapName(name: String): String = name.replace("_", "\\_")
}
