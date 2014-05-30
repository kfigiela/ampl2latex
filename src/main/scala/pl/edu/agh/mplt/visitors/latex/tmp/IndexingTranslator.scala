package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.formula.set.Indexing


class IndexingTranslator extends Translator[Indexing] {

   override def apply(node: Indexing): String = {
      val members = (new IndexingMembersTranslator)(node)
      val lexpr = new StringBuilder(":") append (node.lexpr.map(l => (new LexprTranslator)(l)) getOrElse "")

      s"$members $lexpr"
   }

}
