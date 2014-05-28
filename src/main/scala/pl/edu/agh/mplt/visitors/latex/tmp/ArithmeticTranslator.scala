package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.formula.expression.{ExpressionReduction, Unary, BinaryOperation, ArithmeticOperation}
import pl.edu.agh.mplt.parser.formula.expression.Bin._
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.{Min, Max, Prod, Sum}


class ArithmeticTranslator extends Translator[ArithmeticOperation] {

  override def apply(node: ArithmeticOperation): String = node match {
    case Unary.-(expr)            => s"-${(new ExprTranslator)(expr) }"
    case /(left, right)           => s"\\frac{${(new ExprTranslator)(left) }}{${(new ExprTranslator)(right) }}"
    case bin: BinaryOperation     => translateBinary(bin)
    case red: ExpressionReduction => translateReduction(red)
    case arith                    => s"Unsupprted atithmetic operation: $arith"
  }


  private def translateBinary(bin: BinaryOperation) = {
    val (left, right) = ((new ExprTranslator)(bin.left), (new ExprTranslator)(bin.right))
    val op = getOperator(bin)
    s"$left $op $right"
  }

  private def translateReduction(reduction: ExpressionReduction) = {
    val members = (new IndexingMembersTranslator)(reduction.indexing)
    val op = getOperator(reduction)
    val expr = (new ExprTranslator)(reduction.expr)

    s"$op_{$members}($expr)"
  }

  private def getOperator(operation: ArithmeticOperation) = operation match {
    case +(_, _)    => "+"
    case -(_, -)    => "-"
    case less(-, _) => "less"
    case *(_, _)    => "\\cdot"
    case ^(_, -)    => "^"
    case div(_, _)  => "\\div"
    case mod(_, _)  => "\\mod"

    case Sum(_, _)  => "\\sum"
    case Prod(_, _) => "\\prod"
    case Max(_, _)  => "\\max"
    case Min(_, _)  => "\\min"
  }
}
