package pl.edu.agh.mplt.visitors.latex

import pl.edu.agh.mplt.parser.declaration.data.Attribute
import pl.edu.agh.mplt.parser.declaration.data.Attribute._
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.SetExpression

trait DataAttributeTranslator {
  def translateExpression(expr: Expression): String

  def translateSetExpression(expr: SetExpression): String

  def translateAttribute(attr: Attribute)(name: String): String = attr match {
    case Binary => s"$name \\in {0, 1}"
    case Integer => s"$name \\in \\mathbb{Z}"
    case Symbolic => s"$name is symbolic"
    case Relation(op, expr) => s"$name ${translateOperator(op)} ${translateExpression(expr)}"
    case Inclusion(sexpr) => s"$name \\in {${translateSetExpression(sexpr)}}"
    case Defined(expr) => s"$name := {${translateExpression(expr)}}"
    case Dimension(n) => s"$name \\in \\mathbb{R}^{$n}"
    case Within(sexpr) => s"$name \\subseteq  {${translateSetExpression(sexpr)}}"
    case DefaultValue(expr) => s"$name = {${translateExpression(expr)}}"
    case FinalValue(expr) => s"$name = {${translateExpression(expr)}}"
    case DefaultSet(sexpr) => s"$name = {${translateSetExpression(sexpr)}}"
    case FinalSet(sexpr) => s"$name = {${translateSetExpression(sexpr)}}"
    case Coefficient(_, _, _) => ""
    case Objective(_, _, _) => ""
    case Cover(_, _) => ""
  }

  private def translateOperator(op: String): String = op match {
    case "!=" => "\\neq"
    case "<=" => "\\le"
    case ">=" => "\\ge"
    case _ => op
  }
}
