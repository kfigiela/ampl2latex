package pl.edu.agh.mplt.visitors.translator.mappers

import scala.collection.mutable

import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.phrase.expression.Bin._
import pl.edu.agh.mplt.parser.phrase.logical.{LogicalExpression, ParenthesizedLogical}
import pl.edu.agh.mplt.parser.phrase.logical.Logical._
import pl.edu.agh.mplt.visitors.NodeMapper

class AddNecessaryParenthesis(operations: mutable.Buffer[NodeMapper] = mutable.Buffer())
extends NodeMapper(operations) {

   override def mapExpr(expr: Expression): Expression = expr match {
      case Bin./(left, right) =>
         Bin./(mapExpr(left), mapExpr(right)) //top level fraction expressions don't need to be parenthesized

      case bin: BinaryOperation => parenthesizeBinary(bin)

      case e => super.mapExpr(e)
   }

   override def mapLexpr(lexpr: LogicalExpression): LogicalExpression = lexpr match {
      case n@Not(_)    => parenthesizeLogic(n)
      case o@Or(_, _)  => parenthesizeLogic(o)
      case a@And(_, _) => parenthesizeLogic(a)

      case l => super.mapLexpr(l)
   }

   private def parenthesizeLogic(lexpr: LogicalExpression): LogicalExpression = lexpr match {
      case o@Or(Or(_, _), Or(_, _)) => o
      case Or(l@Or(_, _), r)        => Or(l, ParenthesizedLogical(r))
      case Or(l, r@Or(_, _))        => Or(ParenthesizedLogical(l), r)
      case Or(l, r)                 => Or(ParenthesizedLogical(l), ParenthesizedLogical(r))

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
         val left = if (lPriority < priority) ParenthesizedExpression(binLeft) else binLeft

         val right = if (f(rPriority, priority)) ParenthesizedExpression(binRight) else binRight

         copyBinary(bin)(left, right)
      }

      assoc match {
         case Expression.Left  => parenthesizeWith(_ < _)
         case Expression.Right => parenthesizeWith(_ <= _)

         case a => throw new Error(s"Unexpected: $a")
      }
   }

   private def copyBinary(bin: BinaryOperation)(left: Expression, right: Expression): BinaryOperation =
      bin match {
         case +(_, _)    => Bin.+(left, right)
         case -(_, _)    => Bin.-(left, right)
         case *(_, _)    => Bin.*(left, right)
         case /(_, _)    => Bin./(left, right)
         case ^(_, _)    => Bin.^(left, right)
         case Div(_, _)  => Bin.Div(left, right)
         case Mod(_, _)  => Bin.Mod(left, right)
         case Less(_, _) => Bin.Less(left, right)

         case ExpressionIf(cond, _, _) => ExpressionIf(cond, left, right)

         case e => throw new Error(s"Unexpected token: $e")
      }
}
