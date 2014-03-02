package pl.edu.agh.mplt.parser.expression

trait Expression

case class Parenthesized(expr: Expression) extends Expression



