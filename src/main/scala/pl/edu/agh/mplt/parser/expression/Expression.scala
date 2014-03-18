package pl.edu.agh.mplt.parser.expression

import pl.edu.agh.mplt.parser.ASTNode

trait Expression extends ASTNode

case class Parenthesized(expr: Expression) extends Expression

case class Number(v: String) extends Expression

case class StringLiteral(str: String) extends Expression

