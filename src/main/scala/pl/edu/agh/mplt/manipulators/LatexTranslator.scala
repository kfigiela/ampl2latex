package pl.edu.agh.mplt.manipulators

import pl.edu.agh.mplt.manipulators.latex._
import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.parser.declaration.datatype.DatatypeDeclaration

class LatexTranslator extends Manipulator with DataDeclarationTranslator with ExpressionTranslator
with SetExpressionTranslator with LogicalExpressionTranslator with MemberTranslator {

  override def translateDeclaration(declaration: Declaration): String = declaration match {
    case c: ConstraintDeclaration => translateConstraint(c)
    case o: ObjectiveDeclaration => translateObjective(o)
    case d: DatatypeDeclaration => translateData(d)
    case a: Assertion => translateAssertion(a)
  }

  def translateConstraint(constraintDeclaration: ConstraintDeclaration): String = ""

  def translateAssertion(assertion: Assertion): String = ""

  def translateObjective(objective: ObjectiveDeclaration): String = ""

}
