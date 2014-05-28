package pl.edu.agh.mplt.visitors.latex.mappers

import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.formula.expression.{ParenthesizedExpression, BinaryOperation, Expression, ArithmeticOperation}
import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.formula.set.ParenthesizedSetExpression
import pl.edu.agh.mplt.parser.formula.logical.ParenthesizedLogical


class StripAllParenthesis[A <: ASTNode](val operations: List[NodeMapper] = Nil) extends NodeMapper[A] {

  override def apply(node: A): ASTNode = node match {
    case ParenthesizedExpression(expr) => expr
    case ParenthesizedSetExpression(sexpr) => sexpr
    case ParenthesizedLogical(lexpr) => lexpr
    case n => n
  }
}
