package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.objective.{Minimize, Maximize, ObjectiveDeclaration}
import pl.edu.agh.mplt.visitors.translator.Translator


class ObjectiveTranslator extends Translator[ObjectiveDeclaration] {
   override def apply(node: ObjectiveDeclaration): String = {
      val name = node.name
      val members: String = node.indexing.map(i => (new IndexingMembersTranslator)(i)).map(ms => s"_{$ms}") getOrElse ""
      val expression: String = node.expression.map(e => (new ExprTranslator)(e)).map(e => s"{$e}") getOrElse ""

      val obj = node match {
         case Maximize(_, _, _, _) => s"& \\underset{${name}}{\\text{maximize}}"
         case Minimize(_, _, _, _) => s"& \\underset{${name}}{\\text{minimize}}"
      }

      s"${obj }$members $expression"
   }
}
