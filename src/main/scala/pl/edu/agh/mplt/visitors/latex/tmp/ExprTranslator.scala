package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.formula.expression.ParenthesizedExpression
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.visitors.latex.mappers.StripAllParenthesis
import pl.edu.agh.mplt.parser.reference.Reference


class ExprTranslator extends Translator[Expression] {

   override def apply(node: Expression): String = node match {
      case Number(nr)                     => nr
      case ParenthesizedExpression(expr)  => s"(${(new ExprTranslator)(node) }})"
      case i@ExpressionIf(_, _, _)        => translatorIf(i)
      case f@FunctionCall(name, args)     => translateFunction(f)
      case p@PiecewiseLinearTerm(_, _, _) => ""

      case a: ArithmeticOperation => (new ArithmeticTranslator(this))(a)
      case ref: Reference         => (new ReferenceTranslator)(ref)

      case e => throw new Error(s"Unsupported Expression: $e")
   }

   private def translateFunction(f: FunctionCall) = {
      val args = f.args.map(e => (new ExprTranslator)(e))
      val arguments = (args.head /: args.tail)(_ + "," + _)

      s"${f.name }($arguments)"
   }

   private def translatorIf(expr: ExpressionIf): String = {
      def parenthesize(expr: Expression) = expr match {
         case e@ExpressionIf(_, _, _) => s"(${(new ExprTranslator)(e) }})"
         case e                       => (new ExprTranslator)(e)
      }

      val cond = (new LexprTranslator)(expr.lexpr)

      val hasNestedIfs = (new NestedIfChecker)(expr.left) || (new NestedIfChecker)(expr.left)

      if (hasNestedIfs) {
         s"if($cond) then: {${parenthesize(expr.left) }} else: {${parenthesize(expr.right) }}"
      } else {
         val (t, f) = ((new ExprTranslator)(expr.left), (new ExprTranslator)(expr.left))
         bracketedConditional(cond, t, f)
      }
   }

   private def bracketedConditional(cond: String, t: String, f: String) =
      s"""
      |\begin{cases}
      |    \$t,& \text{if } $cond
      |    $f,              & \text{otherwise}
      |\end{cases}
    """.stripMargin
}
