package pl.edu.agh.mplt.parser.formula.expression

import pl.edu.agh.mplt.parser.formula.Formula
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression

trait Expression extends Formula

case class ParenthesizedExpression(expr: Expression) extends Expression

case class Number(v: String) extends Expression

case class ExpressionIf(lexpr: LogicalExpression, trueBranch: Expression,
              falseBranch: Option[Expression] = None) extends Expression

sealed trait ArithmeticOperation extends Expression

object Bin {

  case class +(l: Expression, r: Expression) extends ArithmeticOperation

  case class -(l: Expression, r: Expression) extends ArithmeticOperation

  case class less(l: Expression, r: Expression) extends ArithmeticOperation

  case class *(l: Expression, r: Expression) extends ArithmeticOperation

  case class /(l: Expression, r: Expression) extends ArithmeticOperation

  case class div(l: Expression, r: Expression) extends ArithmeticOperation

  case class mod(l: Expression, r: Expression) extends ArithmeticOperation

  case class ^(l: Expression, r: Expression) extends ArithmeticOperation

}

object Unary {

  case class -(e: Expression) extends ArithmeticOperation

}

object ExpressionReduction {

  case class Sum(indexing: Indexing, expr: Expression) extends ArithmeticOperation

  case class Prod(indexing: Indexing, expr: Expression) extends ArithmeticOperation

  case class Max(indexing: Indexing, expr: Expression) extends ArithmeticOperation

  case class Min(indexing: Indexing, expr: Expression) extends ArithmeticOperation

}