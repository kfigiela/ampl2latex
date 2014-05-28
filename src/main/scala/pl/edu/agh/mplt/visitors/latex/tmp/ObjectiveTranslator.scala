package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.objective.{Minimize, Maximize, ObjectiveDeclaration}


class ObjectiveTranslator extends Translator[ObjectiveDeclaration] {
  override def apply(node: ObjectiveDeclaration): String = {
    val members = (new IndexingMembersTranslator)(node.indexing)
    val expression = (new ExpressionTranslator)(node.expression)

    val obj = node match {
      case Maximize(_, _, _, _) => "max"
      case Minimize(_, _, _, _) => "minimize"
    }

    s"${obj }_{$members} ($expression)"
  }
}
