package pl.edu.agh.mplt.parser.formula.expression.arithmetic

import pl.edu.agh.mplt.parser.formula.expression.Expression

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
