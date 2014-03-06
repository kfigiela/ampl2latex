package pl.edu.agh.mplt.parser.expression

import pl.edu.agh.mplt.parser.IntercodeExpression

trait Expression extends IntercodeExpression

case class Parenthesized(expr: Expression) extends Expression

case class Number(v: String) extends Expression

case class StringLiteral(str: String) extends Expression

