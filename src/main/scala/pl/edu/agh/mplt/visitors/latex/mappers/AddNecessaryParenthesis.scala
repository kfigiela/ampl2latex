package pl.edu.agh.mplt.visitors.latex.mappers

import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.formula.expression.Bin._
import scala.collection.mutable
import pl.edu.agh.mplt.parser.formula.logical.Logical.{and, or, not}
import pl.edu.agh.mplt.parser.formula.logical.{ParenthesizedLogical, LogicalExpression}


class AddNecessaryParenthesis(operations: mutable.Seq[NodeMapper] = Nil)
      extends NodeMapper(operations) {

   override def mapExpr(expr: Expression): Expression = expr match {
      case bin: BinaryOperation => parenthesizeBinary(bin)
      case e                    => super.mapExpr(e)
   }

   override def mapLexpr(lexpr: LogicalExpression): LogicalExpression = lexpr match {
      case n@not(_)    => parenthesizeLogic(n)
      case o@or(_, _)  => parenthesizeLogic(o)
      case a@and(_, _) => parenthesizeLogic(a)
   }

   private def parenthesizeLogic(lexpr:LogicalExpression):LogicalExpression = lexpr match {
      case o@or(or(_, _), or(_, _)) => o
      case or(l@or(_, _), r) => or(l, ParenthesizedLogical(r))
      case or(l, r@or(_, _)) => or(ParenthesizedLogical(l), r)
      case or(l, r) => or(ParenthesizedLogical(l), ParenthesizedLogical(r))

      case log => log
   }



   private def parenthesizeBinary(bin: BinaryOperation): Expression = {
      val binLeft = mapExpr(bin.left)
      val binRight = mapExpr(bin.right)

      val assoc = Expression.associativity(bin)
      val priority = Expression.priority(bin)

      val lPriority = Expression.priority(binLeft)
      val rPriority = Expression.priority(binRight)

      def parenthesizeWith(f: (Int, Int) => Boolean): Expression = {
         val left = if (lPriority < priority) ParenthesizedExpression(binLeft)
         else binLeft

         val right = if (f(rPriority, priority)) ParenthesizedExpression(binRight)
         else binRight

         copyBinary(bin)(left, right)
      }

      assoc match {
         case Left  => parenthesizeWith(_ < _)
         case Right => parenthesizeWith(_ <= _)
      }
   }

   private def copyBinary(bin: BinaryOperation)(left: Expression,
                                                right: Expression): BinaryOperation = bin match {
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
