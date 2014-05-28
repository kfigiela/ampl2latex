package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration


class ConstraintTranslator extends Translator[ConstraintDeclaration]{
  override def apply(node: ConstraintDeclaration): String = {
    val indexing = (new IndexingTranslator)(node.indexing)
    val constraint = (new ConstraintExpressionTranslator)(node.constraint)

    s"$indexing $constraint"
  }
}
