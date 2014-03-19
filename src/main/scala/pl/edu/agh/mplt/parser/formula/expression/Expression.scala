package pl.edu.agh.mplt.parser.formula.expression

import pl.edu.agh.mplt.parser.formula.Formula

trait Expression extends Formula

case class ParenthesizedExpression(expr: Expression) extends Expression

case class Number(v: String) extends Expression
