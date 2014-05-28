package pl.edu.agh.mplt.visitors.latex.tmp


import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.data.DataDeclaration
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.visitors.latex.mappers.NameFixer


class LatexTranslator extends Translator[Declaration] {
  override def apply(node: Declaration): String = node match {
    case a: Assertion             => (new AssertionTranslator)(a)
    case d: DataDeclaration       => (new NameFixer andThen new DataTranslator)(d)
    case o: ObjectiveDeclaration  => (new ObjectiveTranslator)(o)
    case c: ConstraintDeclaration => (new ConstraintTranslator)(c)
    case dec                      => throw new Error(s"Unexpected token: $dec")
  }

}
