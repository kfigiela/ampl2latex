package pl.edu.agh.mplt.parser.phrase.expression

import pl.edu.agh.mplt.parser.phrase.Phrase
import pl.edu.agh.mplt.parser.phrase.set.Indexing

trait Expression extends Phrase

object Expression {

   import Bin._

   trait Associativity

   case object Left extends Associativity

   case object Right extends Associativity

   def priority(expr: Expression): Int = expr match {
      case +(_, _) | -(_, _) | Less(_, _)            => 1
      case *(_, _) | /(_, _) | Div(_, _) | Mod(_, _) => 2
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


