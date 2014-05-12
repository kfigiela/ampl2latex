package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.visitors.latex._
import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.constraint.ConstraintDeclaration
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.objective.ObjectiveDeclaration
import pl.edu.agh.mplt.parser.declaration.datatype.DatatypeDeclaration
import pl.edu.agh.mplt.parser.ASTNode

class LatexTranslator extends Visitor[String] with DataDeclarationTranslator with ExpressionTranslator
with SetExpressionTranslator with LogicalExpressionTranslator with MemberTranslator with DataAttributeTranslator
with ReferenceTranslator {

  override def visit(declaration: Declaration): String = declaration match {
//    case c: ConstraintDeclaration => translateConstraint(c)
//    case o: ObjectiveDeclaration => translateObjective(o)
    case d: DatatypeDeclaration => translateData(d)
    case _ => ""
//    case a: Assertion => translateAssertion(a)
  }

  private def translateConstraint(constraintDeclaration: ConstraintDeclaration): String = ""

  private def translateAssertion(assertion: Assertion): String = ""

  private def translateObjective(objective: ObjectiveDeclaration): String = ""

  def reduce[A <: ASTNode](begin: String, end: String)(delim: String)(list: Traversable[A], f: A => String): String =
    begin + {
      list match {
        case Nil => ""
        case hd :: tl => (f(hd) /: tl)(_ + delim + f(_))
      }
    } + end

}
