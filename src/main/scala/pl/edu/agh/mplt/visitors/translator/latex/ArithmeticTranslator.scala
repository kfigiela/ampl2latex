package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.phrase.expression.Bin._
import pl.edu.agh.mplt.parser.phrase.expression.ExpressionReduction._
import pl.edu.agh.mplt.visitors.translator.Translator


class ArithmeticTranslator(translate: ExprTranslator) extends Translator[ArithmeticOperation] {

   override def apply(node: ArithmeticOperation): String = node match {
      case Unary.-(expr)  => s"-(${translate(expr) })"
      case /(left, right) => s"\\frac{${translate(left) }}{${translate(right) }}"

      case bin: BinaryOperation     => translateBinary(bin)
      case red: ExpressionReduction => translateReduction(red)

      case arith => s"Unsupprted atithmetic operation: $arith"
   }


   private def translateBinary(bin: BinaryOperation) = {
      val left = translate(bin.left)
      val right = translate(bin.right)
      val op = getOperator(bin)

      s"$left $op $right"
   }

   private def translateReduction(reduction: ExpressionReduction) = {
      val members = (new IndexingMembersTranslator)(reduction.indexing)
      val op = getOperator(reduction)
      val expr = translate(reduction.expr)

      s"${op }_{$members}($expr)"
   }

   private def getOperator(operation: ArithmeticOperation) = operation match {
      case Unary.-(_) => "-"

      case +(_, _)    => "+"
      case -(_, _)    => "-"
      case Less(_, _) => "less"

      case *(_, _)   => "\\cdot"
      case /(_, _)   => ":"
      case Div(_, _) => "\\div"
      case Mod(_, _) => "\\mod"

      case ^(_, _) => "^"

      case Sum(_, _)  => "\\sum"
      case Prod(_, _) => "\\prod"
      case Max(_, _)  => "\\max"
      case Min(_, _)  => "\\min"

      case e@ExpressionIf(_, _, _) => throw new Error(s"Unexpected token: $e")
   }
}
