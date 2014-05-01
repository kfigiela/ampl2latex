package pl.edu.agh.mplt.manipulators.latex

import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression

trait SetExpressionTranslator {

  import Sets._

  def translateMember(member: Member): String

  def translateLogicalExpression(lexpr: LogicalExpression): String

  def translateSetExpression(sexpr: SetExpression): String =
    sexpr match {
      case ParenthesizedSetExpression(sexpr) => "(" + translateSetExpression(sexpr) + ")"
      case SetOf(indexing, member) => s"{ ${translateMember(member)} | ${translateSetExpression(indexing)}"
      case Union(left, right) => s"{${translateSetExpression(left)} \\bigcup ${translateSetExpression(right)}"
      case Intersection(left, right) => s"{${translateSetExpression(left)} \\bigcap ${translateSetExpression(right)}"
      case Difference(left, right) => s"{${translateSetExpression(left)} \\setminus ${translateSetExpression(right)}"
      case SymetricDifference(left, right) => s"{${translateSetExpression(left)} \\oplus ${translateSetExpression(right)}"
      case Cartesian(left, right) => s"{${translateSetExpression(left)} \\times ${translateSetExpression(right)}"
      case SetExpressionIf(c, t, f) => s"if (${translateLogicalExpression(c)} then: ${translateSetExpression(t)} else: ${translateSetExpression(f)}})"
      case IndexedSet(indexes, sexpr) => s"{${(indexes.head /: indexes.tail)(_ + ", " + _)} \\in ${translateSetExpression(sexpr)}}"
      case l@Indexing(_, _) => translateIndexing(l)
      case s: SetLiteral => translateLiteral(s)
    }

  private def translateLiteral(lit: SetLiteral): String = lit match {
    case ExplicitSet(members) => members.find(_ => true) match {
      case Some(member) => (translateMember(member) /: (members - member))(_ + ", " + translateMember(_))
      case _ => "{}"
    }
    case SetComprehension(start, end, step) => s"[ x | x \\in [$start, $end] \\land $step \\mid x ]"

  }

  def translateIndexing(ind: Indexing): String = ind match {
    case Indexing(sexprs, lexpr) => {
      val tmp = sexprs.map(translateSetExpression).reverse
      val args = (tmp.head /: tmp.tail) {
        case (inner, outer) => s"$outer \\atop {$inner}"
      }
      val cond = lexpr match {
        case None => ""
        case Some(logical) => translateLogicalExpression(logical)
      }
      s"""\\bigwedge {$args \\atop {$cond}}"""
    }
  }
}
