package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.phrase.expression.ParenthesizedExpression
import pl.edu.agh.mplt.parser.phrase.expression.Number
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.visitors.translator.Translator


class ExprTranslator extends Translator[Expression] {

   override def apply(node: Expression): String = node match {
      case Number(nr)                    => nr
      case ParenthesizedExpression(expr) => s"(${(new ExprTranslator)(expr) })"
      case i@ExpressionIf(_, _, _)       => translateIf(i)
      case f@FunctionCall(name, args)    => translateFunction(f)

      case a: ArithmeticOperation => (new ArithmeticTranslator(this))(a)
      case ref: Reference         => (new ReferenceTranslator)(ref)

      case e => throw new Error(s"Unsupported Expression: $e")
   }

   private def translateFunction(f: FunctionCall) = {
      val args = f.args.map(e => (new ExprTranslator)(e))
      val arguments = joinWith(", \\ ")(args)

      s"${f.name }($arguments)"
   }

   private def translateIf(expr: ExpressionIf): String = {
      val cond = (new LexprTranslator)(expr.lexpr)

      val t = apply(expr.left)
      val f = apply(expr.right)

      bracketedConditional(cond, t, f)
   }


}
