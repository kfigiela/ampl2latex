package pl.edu.agh.mplt.parser.formula.expression

import pl.edu.agh.mplt.parser.formula.Formula

trait Expression extends Formula

case class ParenthesizedExpression(expr: Expression) extends Expression

case class Number(v: String) extends Expression

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