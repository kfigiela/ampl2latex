package pl.edu.agh.mplt.parser

import pl.edu.agh.mplt.parser.member.{ExpressionMember, StringMember, SetMember}
import pl.edu.agh.mplt.parser.phrase.expression.{Number, Expression}
import pl.edu.agh.mplt.parser.phrase.set.SetExpression
import pl.edu.agh.mplt.parser.reference.SimpleReference

trait IntercodeImplicits {
  implicit def intToString(i: Int): String = i.toString

  implicit def intToNumber(i: Int): Number = Number(i.toString)

  implicit def doubleToNumber(i: Double): Number = Number(i.toString)

  implicit def intToMember(i: Int): SetMember = ExpressionMember(i)

  implicit def doubleToMember(i: Double): SetMember = ExpressionMember(i)

  implicit def stringToStringMember(str: String): StringMember = StringMember(str)

  implicit def stringToMember(str: String): SetMember = StringMember(str)

  implicit def stringToReference(id: String): SimpleReference = SimpleReference(id)

  implicit def sexprToListOfSexprs(sexpr: SetExpression): List[SetExpression] = List(sexpr)

  implicit def exprToMember(expr: Expression): SetMember = ExpressionMember(expr)

}
