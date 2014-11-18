package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.phrase.set.Indexing
import pl.edu.agh.mplt.visitors.translator.Translator

class IndexingMembersTranslator extends Translator[Indexing] {
  override def apply(node: Indexing): String = {
    val translatedMembers = node.sexprs.map(s => (new SexprTranslator)(s)).reverse

    var ss = translatedMembers.reverse
    val sb = new StringBuilder(" ")
    while (ss.nonEmpty) {
      val hd = ss.head
      val tl = ss.tail

      sb.append(hd)
      if (tl.nonEmpty)
        sb.append("\\atop {")

      ss = tl
    }
    sb.append("}" * (translatedMembers.size - 1))

    sb.toString()
  }

}
