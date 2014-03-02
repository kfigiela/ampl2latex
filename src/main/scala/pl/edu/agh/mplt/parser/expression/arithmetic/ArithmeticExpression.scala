package pl.edu.agh.mplt.parser.expression.arithmetic

import pl.edu.agh.mplt.parser.expression.Expression

sealed trait ArithmeticExpression extends Expression

object Bin {

  case class +(l: Expression, r: Expression) extends ArithmeticExpression

  case class -(l: Expression, r: Expression) extends ArithmeticExpression

  case class less(l: Expression, r: Expression) extends ArithmeticExpression

  case class *(l: Expression, r: Expression) extends ArithmeticExpression

  case class /(l: Expression, r: Expression) extends ArithmeticExpression

  case class div(l: Expression, r: Expression) extends ArithmeticExpression

  case class mod(l: Expression, r: Expression) extends ArithmeticExpression

  case class ^(l: Expression, r: Expression) extends ArithmeticExpression

}

object Unary {

  case class -(e: Expression) extends ArithmeticExpression

}

