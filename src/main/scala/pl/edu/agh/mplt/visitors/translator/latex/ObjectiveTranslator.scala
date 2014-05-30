package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.objective.{Minimize, Maximize, ObjectiveDeclaration}
import pl.edu.agh.mplt.visitors.translator.Translator


class ObjectiveTranslator extends Translator[ObjectiveDeclaration] {
   override def apply(node: ObjectiveDeclaration): String = {
      val name = node.name
      val members: String = node.indexing.map(i => (new IndexingMembersTranslator)(i)) getOrElse " "
      val expression: String = node.expression.map(e => (new ExprTranslator)(e)) getOrElse ""

      val obj = node match {
         case Maximize(_, _, _, _) => "max"
         case Minimize(_, _, _, _) => "min"
      }

      s"$name: ${obj }_{$members} {$expression}"
   }
}