package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.objective.{Minimize, Maximize, ObjectiveDeclaration}


class ObjectiveTranslator extends Translator[ObjectiveDeclaration] {
  override def apply(node: ObjectiveDeclaration): String = {
    val members: String = node.indexing.map(i => (new IndexingMembersTranslator)(i)) getOrElse ""
    val expression: String = node.expression.map(e => (new ExpressionTranslator)(e))

    val obj = node match {
      case Maximize(_, _, _, _) => "max"
      case Minimize(_, _, _, _) => "min"
    }

    s"${obj }_{$members} ($expression)"
  }
}
