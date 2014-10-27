package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.data.DataDeclaration
import pl.edu.agh.mplt.visitors.translator.Translator
import pl.edu.agh.mplt.parser.phrase.set.{IndexedSet, Indexing}


class DataTranslator extends Translator[DataDeclaration] {

   override def apply(node: DataDeclaration): String = {
      val name = node.name
      val indexedName = node.indexing.map(zipWithIndexes(name, _)) getOrElse name
      val indexing: String = node.indexing.map((new IndexingTranslator)(_)) getOrElse ""

      val attrs = joinWith(",")(node.attributes.map((new AttributeTranslator(indexedName))(_)))

      s"$name: $indexing $attrs"
   }

   private def zipWithIndexes(name: String, indexing: Indexing): String = {
      val indices = joinWith(",")(indexing.sexprs.flatMap{case IndexedSet(is, _) => is })

      if(indices != "" && indices != " ") s"${name}_{$indices}"
      else name
   }

}
