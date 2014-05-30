package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.visitors.translator.Translator


class IndexingMembersTranslator extends Translator[Indexing] {
   override def apply(node: Indexing): String = {
      val translatedMembers = node.sexprs.map(s => (new SexprTranslator)(s)).reverse

      translatedMembers match {
         case Nil      => " "
         case hd :: tl => (hd /: tl)(atop)
      }
   }

   def atop(above: String, below: String): String = s"$above \\atop {$below}"
}
