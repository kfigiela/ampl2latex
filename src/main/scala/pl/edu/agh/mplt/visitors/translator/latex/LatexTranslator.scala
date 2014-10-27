package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.{InvalidDeclaration, Declaration}
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.data.DataDeclaration
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.visitors.translator.Translator


class LatexTranslator extends Translator[Declaration] {
   override def apply(node: Declaration): String = node match {
      case a: Assertion             => (new AssertionTranslator)(a)
      case d: DataDeclaration       => (new DataTranslator)(d)
      case o: ObjectiveDeclaration  => (new ObjectiveTranslator)(o)
      case c: ConstraintDeclaration => (new ConstraintTranslator)(c)

      case InvalidDeclaration(msg) => s"INVALID DECLARATION: {$msg}"

      case dec => throw new Error(s"Unexpected token: $dec")
   }

}
