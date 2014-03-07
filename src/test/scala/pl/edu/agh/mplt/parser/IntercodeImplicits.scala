package pl.edu.agh.mplt.parser

import pl.edu.agh.mplt.parser.expression.{StringLiteral, Number}
import pl.edu.agh.mplt.parser.expression.set.SetExpression

trait IntercodeImplicits {
  implicit def intToString(i: Int): String = i.toString

  implicit def intToNumber(i: Int): Number = Number(i.toString)

  implicit def doubleToNumber(i: Double): Number = Number(i.toString)

  implicit def stringToStringLiteral(str: String): StringLiteral = StringLiteral(str)

  implicit def sexprToListOfSexprs(sexpr: SetExpression): List[SetExpression] = List(sexpr)

}
