package pl.edu.agh.mplt.visitors.translator.mappers

import scala.collection.mutable
import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.phrase.set.{IndexedSet, SetExpression, Indexing}
import pl.edu.agh.mplt.parser.member.{MultiMember, ExpressionMember, SetMember}


class IndexingFixer(operations: mutable.Buffer[NodeMapper] = mutable.Buffer()) extends NodeMapper(operations) {
   override def mapIndexing(index: Indexing): Indexing = index match {
      case Indexing(sexprs, lexpr) => Indexing(addDefaultIndices(sexprs).map(mapSexpr), lexpr.map(mapLexpr))

      case e => super.mapIndexing(e)
   }

   def addDefaultIndices(sexprs: List[SetExpression]):List[SetExpression] = {
      var i = 0
      sexprs map {
         case s@IndexedSet(_, _) => s
         case s =>
            i += 1
            IndexedSet(List(s"i_$i"), s)
      }
   }

}
