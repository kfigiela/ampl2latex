package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.phrase.set.Indexing
import pl.edu.agh.mplt.visitors.translator.Translator


class IndexingTranslator extends Translator[Indexing] {

   override def apply(node: Indexing): String = {
      val members = (new IndexingMembersTranslator)(node)
      val lexpr = node.lexpr.map(l => (new LexprTranslator)(l)) getOrElse ""

      if (lexpr == null || lexpr =="")
        s"\\mathop\\forall_{$members}"
     else
        s"\\mathop\\forall_{$members\\atop{$lexpr}}"
   }

}
