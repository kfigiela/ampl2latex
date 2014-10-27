package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.visitors.translator.Translator


class ConstraintTranslator extends Translator[ConstraintDeclaration] {
   override def apply(node: ConstraintDeclaration): String = {
      val name = node.name
      val indexing = node.indexing.map(i => (new IndexingTranslator)(i)) getOrElse ""
      val constraint = (new ConstraintExpressionTranslator)(node.constraint)

      s"$indexing $constraint"
   }
}
