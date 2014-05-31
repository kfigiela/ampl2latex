package pl.edu.agh.mplt.parser.phrase.expression

import pl.edu.agh.mplt.parser.phrase.logical.LogicalExpression
import pl.edu.agh.mplt.parser.phrase.set.Indexing

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

   case class Less(left: Expression, right: Expression) extends BinaryOperation

   case class *(left: Expression, right: Expression) extends BinaryOperation

   case class /(left: Expression, right: Expression) extends BinaryOperation

   case class Div(left: Expression, right: Expression) extends BinaryOperation

   case class Mod(left: Expression, right: Expression) extends BinaryOperation

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