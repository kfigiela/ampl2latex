package pl.edu.agh.mplt.visitors.latex.mappers


import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.formula.logical.{ParenthesizedLogical, LogicalExpression}
import scala.collection.mutable

class StripAllParenthesis(operations: mutable.Seq[NodeMapper] = mutable.Seq()) extends NodeMapper(operations) {
  override def mapExpr(expr: Expression): Expression = expr match {
    case ParenthesizedExpression(e) => e
    case e                          => super.mapExpr(e)
  }

  override def mapSexpr(sexpr: SetExpression): SetExpression = sexpr match {
    case ParenthesizedSetExpression(s) => s
    case s                             => super.mapSexpr(s)
  }

  override def mapLexpr(lexpr: LogicalExpression): LogicalExpression = lexpr match {
    case ParenthesizedLogical(l) => l
    case l                       => super.mapLexpr(l)
  }
}
