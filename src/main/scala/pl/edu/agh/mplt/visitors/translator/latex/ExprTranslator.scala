package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.phrase.expression.ParenthesizedExpression
import pl.edu.agh.mplt.parser.phrase.expression.Number
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.visitors.translator.Translator


class ExprTranslator extends Translator[Expression] {

   override def apply(node: Expression): String = node match {
      case Number("Infinity")             => "\\infty"
      case Number(nr)                     => nr
      case ParenthesizedExpression(expr)  => s"(${(new ExprTranslator)(expr) })"
      case i@ExpressionIf(_, _, _)        => translateIf(i)
      case f@FunctionCall(name, args)     => translateFunction(f)
      case p@PiecewiseLinearTerm(_, _, _) => translatePiecewise(p)

      case a: ArithmeticOperation => (new ArithmeticTranslator(this))(a)
      case ref: Reference         => (new ReferenceTranslator)(ref)

      case e => throw new Error(s"Unsupported Expression: $e")
   }

   private def translateFunction(f: FunctionCall) = {
      val args = f.args.map(e => (new ExprTranslator)(e))
      val arguments = joinWith(", \\ ")(args)

      s"${f.name }($arguments)"
   }

   private def translatePiecewise(p: PiecewiseLinearTerm) = {
      val (expr, optZero) = (apply(p.arguments._1), p.arguments._2.map(apply) )
      val bs = p.breakpoints.map {
         case (optInd, expr) => (optInd.map((new IndexingTranslator)(_)).getOrElse(""), apply(expr))
      }
      val fs = p.slopes.map {
         case (optIndex, expr) => (optIndex.map((new IndexingTranslator)(_)).getOrElse(""), apply(expr))
      }

      val buffer = new StringBuilder()
                   .append("\n \\begin{cases}")

      def piecewiseToString(fIndex: String, fExpr: String, bIndex: String, bExpr: String): String = {
         s"$fIndex $expr \\cdot $fExpr, & if \\  $bIndex $expr \\le $bExpr \\\\ \n"
      }

      bs.zip(fs.dropRight(1)).foreach {
         case ((bsIndex, bsExpr), (fsIndex, fsExpr)) =>
            buffer.append(piecewiseToString(fsIndex, fsExpr, bsIndex, bsExpr))
      }

      // appending optional "zero case" amd otherwise case
      optZero.map(zero => buffer.append(s"0, & if \\ $expr = $zero \\\\ \n"))

      buffer.append(s"$expr \\cdot ${fs.last._2}, &  otherwise \\\\ \n")
      .append("\\end{cases}").toString()
   }

   private def translateIf(expr: ExpressionIf): String = {
      val cond = (new LexprTranslator)(expr.lexpr)

      val t = apply(expr.left)
      val f = apply(expr.right)

      bracketedConditional(cond, t, f)
   }


}
