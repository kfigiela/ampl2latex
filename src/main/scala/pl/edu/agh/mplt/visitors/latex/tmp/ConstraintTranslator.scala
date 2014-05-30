package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration


class ConstraintTranslator extends Translator[ConstraintDeclaration] {
   override def apply(node: ConstraintDeclaration): String = {
      val indexing = node.indexing.map(i => (new IndexingTranslator)(i)) getOrElse ""
      val constraint = (new ConstraintExpressionTranslator)(node.constraint)

      s"$indexing $constraint"
   }
}
