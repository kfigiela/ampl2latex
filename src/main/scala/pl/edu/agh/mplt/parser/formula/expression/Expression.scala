package pl.edu.agh.mplt.parser.formula.expression

import pl.edu.agh.mplt.parser.formula.Formula
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression
import pl.edu.agh.mplt.parser.formula.set.Indexing

trait Expression extends Formula

object Expression {

   import Bin._

   trait Associativity

   case object Left extends Associativity

   case object Right extends Associativity

   def priority(expr: Expression): Int = expr match {
      case +(_, _) | -(_, _) | less(_, _)            => 1
      case *(_, _) | /(_, _) | div(_, _) | mod(_, _) => 2
      case FunctionCall(_, _)                        => 3
      case ^(_, _)                                   => 4
      case ExpressionIf(_, _, _)                     => 5
      case _                                         => 128 //based on fair dice role
   }

   def associativity(expr: BinaryOperation): Associativity = expr match {
      case ExpressionIf(_, _, _) | -(_, _) | ^(_, _) => Right
      case _                                         => Left
   }
}


case class ParenthesizedExpression(expr: Expression) extends Expression

case class Number(v: String) extends Expression


case class FunctionCall(name: String, args: List[Expression]) extends Expression

case class PiecewiseLinearTerm(breakpoints: List[(Option[Indexing], Expression)],
                               slopes: List[(Option[Indexing], Expression)],
                               arguments: (Expression, Option[Expression])) extends Expression

sealed trait ArithmeticOperation extends Expression

sealed trait BinaryOperation extends ArithmeticOperation {
   def left: Expression

   def right: Expression
}

case class ExpressionIf(lexpr: LogicalExpression,
                        left: Expression,
                        right: Expression = Number("0")) extends BinaryOperation

object Bin {

   case class +(left: Expression, right: Expression) extends BinaryOperation

   case class -(left: Expression, right: Expression) extends BinaryOperation

   case class less(left: Expression, right: Expression) extends BinaryOperation

   case class *(left: Expression, right: Expression) extends BinaryOperation

   case class /(left: Expression, right: Expression) extends BinaryOperation

   case class div(left: Expression, right: Expression) extends BinaryOperation

   case class mod(left: Expression, right: Expression) extends BinaryOperation

   case class ^(left: Expression, right: Expression) extends BinaryOperation

}

object Unary {

   case class -(expr: Expression) extends ArithmeticOperation

}

sealed trait ExpressionReduction extends ArithmeticOperation {
   def indexing: Indexing

   def expr: Expression
}

object ExpressionReduction {

   case class Sum(indexing: Indexing, expr: Expression) extends ExpressionReduction

   case class Prod(indexing: Indexing, expr: Expression) extends ExpressionReduction

   case class Max(indexing: Indexing, expr: Expression) extends ExpressionReduction

   case class Min(indexing: Indexing, expr: Expression) extends ExpressionReduction

}