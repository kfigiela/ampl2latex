package pl.edu.agh.mplt.manipulators.latex

import pl.edu.agh.mplt.parser.member.{StringMember, ExpressionMember, MultiMember, Member}
import pl.edu.agh.mplt.parser.formula.expression.Expression


trait MemberTranslator {
  def translateExpression(expr: Expression): String

  def translateMember(member: Member): String = member match {
    case StringMember(str) => str
    case ExpressionMember(expr) => translateExpression(expr)
    case MultiMember(members) => members match {
      case Nil => "()"
      case hd :: tl => "(" + (translateMember(hd) /: tl)(_ + ", " + translateMember(_)) + ")"
    }
  }
}
