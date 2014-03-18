package pl.edu.agh.mplt.parser.logical.expression

import pl.edu.agh.mplt.parser.logical.LogicalExpression

trait Expression extends LogicalExpression

case class ParenthesizedExpression(expr: Expression) extends Expression

case class Number(v: String) extends Expression
