package pl.edu.agh.mplt.visitors.latex

import pl.edu.agh.mplt.parser.formula.logical._
import pl.edu.agh.mplt.parser.formula.set.{SetExpression, Indexing}
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.expression.Expression

import language.implicitConversions
import pl.edu.agh.mplt.parser.reference.Reference

trait LogicalExpressionTranslator {

  def translateIndexing(indexing: Indexing): String

  def translateSetExpression(sexpr: SetExpression): String

  def translateExpression(expr: Expression): String

  def translateMember(member: Member): String
  def translateRef(ref: Reference): String
  import Logical._
  import LogicalReduction._

  def expressionPriority(expr: Expression): Int

  implicit def logicalPriority(lexpr: LogicalExpression): Int = lexpr match {
    case not(_) => 1
    case Inclusion.subset(_, _) | Exclusion.subset(_, _) => 2
    case Inclusion.member(_, _) | Exclusion.member(_, _) => 3
    case Comparision.<(_, _) | Comparision.<=(_, _) |
         Comparision.>(_, _) | Comparision.>=(_, _) |
         Comparision.==(_, _) | Comparision.!=(_, _) => 4
    case and(_, _) => 5
    case LogicalReduction.Exists(_, _) | LogicalReduction.Forall(_, _) => 6
    case or(_, _) => 7
  case _ => 10
}

  def translateLogicalExpression(lexpr: LogicalExpression): String = lexpr match {
    case ParenthesizedLogical(l) => s"(${translateLogicalExpression(l)})"
      case ref :Reference => translateRef(ref)
    case _ => translateWithPriority(lexpr)(logicalPriority(lexpr))

  }

  def logicalJoinWithDelimeter(left: LogicalExpression, right: LogicalExpression, delimeter: String)
                              (implicit pr: Int): String = {
    def inBrackets(arith: LogicalExpression) = "{" + translateLogicalExpression(arith) + "}"
    val l = if (logicalPriority(left) < pr) inBrackets(left) else translateLogicalExpression(left)
    val r = if (logicalPriority(right) < pr) inBrackets(right) else translateLogicalExpression(right)

    l + delimeter + r
  }

  def translateWithPriority(lexpr: LogicalExpression)(implicit pr: Int): String = {
    lexpr match {
      case Inclusion.member(member, set) => s"${translateMember(member)} \\in {${translateSetExpression(set)}}"
      case Inclusion.subset(subset, set) => s"${translateSetExpression(subset)} \\subseteq {${translateSetExpression(set)}}"
      case Exclusion.member(member, set) => s"${translateMember(member)} \\nsubseteq {${translateSetExpression(set)}}"
      case Exclusion.subset(subset, set) => s"${translateSetExpression(subset)} \\in {${translateSetExpression(set)}}"
      case not(l) => s"~ {$l}"
      case or(l, r) => logicalJoinWithDelimeter(l, r, "\\vee")(pr)
      case and(l, r) => logicalJoinWithDelimeter(l, r, "\\wedge")(pr)
      case Forall(indexing, lexpr) => s"\\exists ${translateIndexing(indexing)}: {${translateLogicalExpression(lexpr)}}"
      case Exists(indexing, lexpr) => s"\\exists ${translateIndexing(indexing)}: {${translateLogicalExpression(lexpr)}}"
      case _ => translateComparision(lexpr)
    }
  }

  def JoinExpressionWithDelimeter(left: Expression, right: Expression, delimeter: String) =
    translateExpression(left) + delimeter + translateExpression(right)

  def translateComparision(lexpr: LogicalExpression): String = {
    lexpr match {
      case Comparision.<=(l, r) => JoinExpressionWithDelimeter(l, r, "\\le")
      case Comparision.<(l, r) => JoinExpressionWithDelimeter(l, r, "<")
      case Comparision.>=(l, r) => JoinExpressionWithDelimeter(l, r, "\\ge")
      case Comparision.>(l, r) => JoinExpressionWithDelimeter(l, r, ">")
      case Comparision.==(l, r) => JoinExpressionWithDelimeter(l, r, "=")
      case Comparision.!=(l, r) => JoinExpressionWithDelimeter(l, r, "\\neq")
      case _ => ""
    }
  }
}
