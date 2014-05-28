package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.formula.set.{SetExpression, Indexing}


class IndexingTranslator extends Translator[Indexing] {

  override def apply(node: Indexing): String = {
    val members = (new IndexingMembersTranslator)(node)
    val lexpr = (new LexprTranslator)(node.lexpr)

    s"$members: $lexpr"
  }

}
