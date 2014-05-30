package pl.edu.agh.mplt.visitors.latex.mappers

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.logical.{Comparision, Logical, ParenthesizedLogical, LogicalExpression}
import pl.edu.agh.mplt.parser.formula.expression.Bin._
import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.declaration.constraint.{ConstraintComparison, ConstraintExpression, BoundedConstraint}
import pl.edu.agh.mplt.parser.declaration.data.Attribute
import pl.edu.agh.mplt.parser.reference.{IndexedReference, Reference}
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.{Min, Max, Prod, Sum}
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Max
import pl.edu.agh.mplt.parser.formula.expression.Bin.*
import pl.edu.agh.mplt.parser.formula.expression.Bin.^
import pl.edu.agh.mplt.parser.formula.expression.Bin.less
import pl.edu.agh.mplt.parser.formula.expression.Bin.mod
import pl.edu.agh.mplt.parser.formula.expression.ParenthesizedExpression
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Prod
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Min
import pl.edu.agh.mplt.parser.formula.set.ParenthesizedSetExpression
import pl.edu.agh.mplt.parser.formula.expression.Bin./
import pl.edu.agh.mplt.parser.formula.set.SetExpressionIf
import pl.edu.agh.mplt.parser.formula.expression.Bin.+
import pl.edu.agh.mplt.parser.formula.expression.Bin.-
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Sum
import pl.edu.agh.mplt.parser.formula.expression.Bin.div
import pl.edu.agh.mplt.parser.formula.set.Sets._
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Max
import pl.edu.agh.mplt.parser.formula.expression.Bin.*
import pl.edu.agh.mplt.parser.formula.expression.Bin.^
import pl.edu.agh.mplt.parser.formula.expression.Bin.less
import pl.edu.agh.mplt.parser.formula.expression.Bin.mod
import pl.edu.agh.mplt.parser.formula.expression.ParenthesizedExpression
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Prod
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Min
import pl.edu.agh.mplt.parser.formula.set.IndexedSet
import pl.edu.agh.mplt.parser.formula.set.ParenthesizedSetExpression
import pl.edu.agh.mplt.parser.formula.expression.Bin./
import pl.edu.agh.mplt.parser.formula.set.SetExpressionIf
import pl.edu.agh.mplt.parser.formula.expression.Bin.+
import pl.edu.agh.mplt.parser.formula.expression.Bin.-
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Sum
import pl.edu.agh.mplt.parser.formula.expression.Bin.div
import pl.edu.agh.mplt.parser.formula.set.Sets.Intersection
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Max
import pl.edu.agh.mplt.parser.formula.set.Sets.SymetricDifference
import pl.edu.agh.mplt.parser.formula.expression.Bin.*
import pl.edu.agh.mplt.parser.formula.expression.Bin.^
import pl.edu.agh.mplt.parser.formula.expression.Bin.less
import pl.edu.agh.mplt.parser.formula.expression.Bin.mod
import pl.edu.agh.mplt.parser.formula.expression.ParenthesizedExpression
import pl.edu.agh.mplt.parser.formula.set.Sets.Union
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Prod
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Min
import pl.edu.agh.mplt.parser.formula.set.IndexedSet
import pl.edu.agh.mplt.parser.formula.set.ParenthesizedSetExpression
import pl.edu.agh.mplt.parser.formula.expression.Bin./
import pl.edu.agh.mplt.parser.formula.set.Sets.Cartesian
import pl.edu.agh.mplt.parser.formula.set.SetExpressionIf
import pl.edu.agh.mplt.parser.formula.expression.Bin.+
import pl.edu.agh.mplt.parser.formula.expression.Bin.-
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction.Sum
import pl.edu.agh.mplt.parser.formula.set.Sets.SetOf
import pl.edu.agh.mplt.parser.formula.set.Sets.Difference
import pl.edu.agh.mplt.parser.formula.expression.Bin.div
import pl.edu.agh.mplt.parser.formula.logical
import pl.edu.agh.mplt.parser.formula.logical.LogicalReduction.{Exists, Forall}
import scala.collection.mutable


class AddNecessaryParenthesis(operations: mutable.Seq[NodeMapper] = Nil) extends NodeMapper(operations){

  override def mapExpr(expr: Expression): Expression = expr match {
    case bin: BinaryOperation => parenthesizeBinary(bin)
    case e                    => super.mapExpr(e)
  }

  private def parenthesizeBinary(bin: BinaryOperation): Expression = {
    val binLeft = mapExpr(bin.left)
    val binRight = mapExpr(bin.right)

    val assoc = Expression.associativity(bin)
    val priority = Expression.priority(bin)

    val (lPriority, rPriority) = (Expression.priority(binLeft), Expression.priority(binRight))

    def parenthesizeWith(f: (Int, Int) => Boolean): Expression = {
      val left = if (lPriority < priority)
        ParenthesizedExpression(binLeft)
      else
        binLeft

      val right = if (f(rPriority, priority))
        ParenthesizedExpression(binRight)
      else
        binRight

      copyBinary(bin)(left, right)
    }

    assoc match {
      case Left  => parenthesizeWith(_ < _)
      case Right => parenthesizeWith(_ <= _)
    }
  }

  private def copyBinary(bin: BinaryOperation)(left: Expression, right: Expression): BinaryOperation = bin match {
    case +(_, _)    => Bin.+(left, right)
    case -(_, _)    => Bin.-(left, right)
    case *(_, _)    => Bin.*(left, right)
    case /(_, _)    => Bin./(left, right)
    case ^(_, _)    => Bin.^(left, right)
    case div(_, _)  => Bin.div(left, right)
    case mod(_, _)  => Bin.mod(left, right)
    case less(_, _) => Bin.less(left, right)

  }
}
