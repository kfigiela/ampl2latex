package pl.edu.agh.mplt.parser

import pl.edu.agh.mplt.parser.member.{ExpressionMember, StringMember, Member}
import pl.edu.agh.mplt.parser.formula.expression.{Number, Expression}
import pl.edu.agh.mplt.parser.formula.set.SetExpression

trait IntercodeImplicits {
  implicit def intToString(i: Int): String = i.toString

  implicit def intToNumber(i: Int): Number = Number(i.toString)

  implicit def doubleToNumber(i: Double): Number = Number(i.toString)

  implicit def stringToStringMember(str: String): StringMember = StringMember(str)

  implicit def sexprToListOfSexprs(sexpr: SetExpression): List[SetExpression] = List(sexpr)

  implicit def intToMember(i: Int): Member = ExpressionMember(i)

  implicit def stringToMember(str: String): Member = StringMember(str)

  implicit def exprToMember(expr: Expression): Member = ExpressionMember(expr)

}
