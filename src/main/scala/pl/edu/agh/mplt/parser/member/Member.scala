package pl.edu.agh.mplt.parser.member

import pl.edu.agh.mplt.parser.logical.expression.Expression

trait Member

case class ExpressionMember(expr: Expression) extends Member

case class StringMember(str: String) extends Member
