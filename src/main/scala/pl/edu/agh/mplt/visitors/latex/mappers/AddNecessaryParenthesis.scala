package pl.edu.agh.mplt.visitors.latex.mappers

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.formula.set.SetExpression
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression
import pl.edu.agh.mplt.parser.formula.expression.Bin._


class AddNecessaryParenthesis(val operations: List[NodeMapper] = Nil) extends NodeMapper[Expression] {

  override def apply(expr: Expression): Expression = expr match {
    case bin: BinaryOperation => parenthesizeBinary(bin)
    case e                    => e
  }

  private def parenthesizeBinary(bin: BinaryOperation): Expression = {
    val assoc = Expression.associativity(bin)
    val priority = Expression.priority(bin)

    val (lPriority, rPriority) = (Expression.priority(bin.left), Expression.priority(bin.right))

    def parenthesizeWith(f: (Int, Int) => Boolean): Expression = {
      val left = if (lPriority < priority)
        ParenthesizedExpression(bin.left)
      else
        bin.left

      val right = if (f(rPriority, priority))
        ParenthesizedExpression(bin.right)
      else
        bin.right

      copyToBinary(left, right)(bin)
    }

    assoc match {
      case Left  => parenthesizeWith(_ < _)
      case Right => parenthesizeWith(_ <= _)
    }
  }



  private def copyToBinary(left: Expression, right: Expression)(bin: BinaryOperation): BinaryOperation = bin match {
    case +(_, _)    => Bin.+(left, right)
    case -(_, _)    => Bin.-(left, right)
    case *(_, _)    => Bin.*(left, right)
    case /(_, _)    => Bin./(left, right)
    case ^(_, _)    => Bin.^(left, right)
    case div(_, _)  => Bin.div(left, right)
    case mod(_, _)  => Bin.mod(left, right)
    case less(_, _) => Bin.less(left, right)

  }
}
