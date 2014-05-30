package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.formula.set.Indexing


class IndexingMembersTranslator extends Translator[Indexing] {
   override def apply(node: Indexing): String = {
      val translatedMembers = node.sexprs.map(s => (new SexprTranslator)(s)).reverse

      (translatedMembers.head /: translatedMembers.tail)(atop)
   }

   def atop(above: String, below: String): String = s"$above \\atop {$below}"
}
