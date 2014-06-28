package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.constraint._
import pl.edu.agh.mplt.parser.declaration.constraint.BoundedConstraint
import pl.edu.agh.mplt.visitors.translator.Translator


class ConstraintExpressionTranslator extends Translator[ConstraintExpression] {
   override def apply(node: ConstraintExpression): String = node match {
      case BoundedConstraint(optLeft, innerExpr, optRight) =>
         val (lop, leftExpr) = optLeft.map(translate) getOrElse("", "")
         val (rop, rightExpr) = optRight.map(translate) getOrElse("", "")
         val expr = (new ExprTranslator)(innerExpr)

         s"$leftExpr $lop $expr $rop $rightExpr"

      case c: ComplementaryConstraint => throw new Error(s"Unsupported constraint: $c")
   }

   private def translate(cc: ConstraintComparison): (String, String) = cc match {
      case Constraint.<=(expr) => ("\\le", (new ExprTranslator)(expr))
      case Constraint.==(expr) => ("=", (new ExprTranslator)(expr))
      case Constraint.>=(expr) => ("\\ge", (new ExprTranslator)(expr))
   }
}
