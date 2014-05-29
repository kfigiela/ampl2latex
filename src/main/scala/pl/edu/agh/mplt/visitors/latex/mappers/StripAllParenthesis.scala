package pl.edu.agh.mplt.visitors.latex.mappers

import scala.Some

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint._
import pl.edu.agh.mplt.parser.declaration.data.{Attribute, ParameterDeclaration, SetDeclaration, VariableDeclaration}
import pl.edu.agh.mplt.parser.declaration.objective.{Maximize, Minimize}
import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.formula.expression.Bin._
import pl.edu.agh.mplt.parser.formula.logical
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.set.Sets._
import pl.edu.agh.mplt.parser.member.{ExpressionMember, Member, MultiMember}
import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.formula.expression.ExpressionReduction._
import logical.LogicalReduction.{Forall, Exists}
import logical._

import pl.edu.agh.mplt.parser.declaration.data.Attribute._
import pl.edu.agh.mplt.parser.reference.Reference

class StripAllParenthesis(val operations: List[NodeMapper] = Nil) extends NodeMapper {

  override def apply(node: ASTNode): ASTNode = node match {
    case ParenthesizedSetExpression(sexpr) => sexpr
    case ParenthesizedExpression(expr)     => expr
    case ParenthesizedLogical(lexpr)       => lexpr

    case Assertion(Some(index), lexpr) =>
      Assertion(Some(apply(index)), apply(lexpr))
    case Assertion(index, lexpr)       => Assertion(index, apply(lexpr))

    case ConstraintDeclaration(n, a, Some(index), constr) =>
      ConstraintDeclaration(n, a, Some(apply(index)), apply(constr))
    case ConstraintDeclaration(n, a, i, constr)           =>
      ConstraintDeclaration(n, a, i, apply(constr))

    case Minimize(n, a, Some(index), Some(expr)) =>
      Minimize(n, a, Some(apply(index)), Some(apply(expr)))
    case Minimize(n, a, Some(index), e)          =>
      Minimize(n, a, Some(apply(index)), e)
    case Minimize(n, a, i, Some(expr))           =>
      Minimize(n, a, i, Some(apply(expr)))

    case Maximize(n, a, Some(index), Some(expr)) =>
      Maximize(n, a, Some(apply(index)), Some(apply(expr)))
    case Maximize(n, a, Some(index), e)          =>
      Maximize(n, a, Some(apply(index)), e)
    case Maximize(n, a, i, Some(expr))           =>
      Maximize(n, a, i, Some(apply(expr)))

    case ParameterDeclaration(n, a, Some(index), attrs) =>
      ParameterDeclaration(n, a, Some(apply(index)), attrs.map(apply))
    case ParameterDeclaration(n, a, Some(index), as)    =>
      ParameterDeclaration(n, a, Some(apply(index)), as)
    case ParameterDeclaration(n, a, i, attrs)           =>
      ParameterDeclaration(n, a, i, attrs.map(apply))

    case VariableDeclaration(n, a, Some(index), attrs) =>
      VariableDeclaration(n, a, Some(apply(index)), attrs.map(apply))
    case VariableDeclaration(n, a, Some(index), as)    =>
      VariableDeclaration(n, a, Some(apply(index)), as)
    case VariableDeclaration(n, a, i, attrs)           =>
      VariableDeclaration(n, a, i, attrs.map(apply))

    case SetDeclaration(n, a, Some(index), attrs) =>
      SetDeclaration(n, a, Some(apply(index)), attrs.map(apply))
    case SetDeclaration(n, a, Some(index), as)    =>
      SetDeclaration(n, a, Some(apply(index)), as)
    case SetDeclaration(n, a, i, attrs)           =>
      SetDeclaration(n, a, i, attrs.map(apply))

    case n => n
  }

  /////////////////
  /// Expression

  override protected def apply(e: Expression): Expression = e match {
    case ExpressionIf(cond, t, f) => ExpressionIf(apply(cond), apply(t), apply(f))

    case f@FunctionCall(_, args) => f.copy(args = args.map(apply))

    case p@PiecewiseLinearTerm(optExpres, optIndexes, (expr, Some(cexpr))) =>
      p.copy(breakpoints = optExpres.map(b => (b._1.map(apply), b._2)),
        slopes = optIndexes.map(s => (s._1.map(apply), s._2)),
        (apply(expr), Some(apply(cexpr))))
    case p@PiecewiseLinearTerm(optExpres, optIndexes, (expr, _))           =>
      p.copy(breakpoints = optExpres.map(b => (b._1.map(apply), b._2)),
        slopes = optIndexes.map(s => (s._1.map(apply), s._2)),
        (apply(expr), None))

    case Unary.-(expr) => Unary.-(apply(expr))

    case bin: BinaryOperation           => apply(bin)
    case reduction: ExpressionReduction => apply(reduction)

  }

  override protected def apply(bin: BinaryOperation): BinaryOperation = bin match {
    case +(left, right)    => Bin.+(apply(left), apply(right))
    case -(left, right)    => Bin.-(apply(left), apply(right))
    case less(left, right) => less(apply(left), apply(right))
    case *(left, right)    => *(apply(left), apply(right))
    case /(left, right)    => /(apply(left), apply(right))
    case div(left, right)  => div(apply(left), apply(right))
    case mod(left, right)  => mod(apply(left), apply(right))
    case ^(left, right)    => ^(apply(left), apply(right))
  }

  override protected def apply(red: ExpressionReduction): ExpressionReduction = red match {
    case Sum(indexing, expr)  => Sum(apply(indexing), apply(expr))
    case Prod(indexing, expr) => Prod(apply(indexing), apply(expr))
    case Max(indexing, expr)  => Max(apply(indexing), apply(expr))
    case Min(indexing, expr)  => Min(apply(indexing), apply(expr))
  }

  /////////////////
  /// SetExpression

  override protected def apply(s: SetExpression): SetExpression = s match {
    case SetExpressionIf(cond, t, f) => SetExpressionIf(apply(cond), apply(t), apply(f))
    case i@IndexedSet(_, sexpr)      => i.copy(sexpr = apply(sexpr))

    case SymetricDifference(s1, s2) => SymetricDifference(apply(s1), apply(s2))
    case Intersection(s1, s2)       => Intersection(apply(s1), apply(s2))
    case Difference(s1, s2)         => Difference(apply(s1), apply(s2))
    case Cartesian(s1, s2)          => Cartesian(apply(s1), apply(s2))
    case SetOf(index, m)            => SetOf(apply(index), apply(m))
    case Union(s1, s2)              => Union(apply(s1), apply(s2))
  }

  override protected def apply(index: Indexing): Indexing = index match {
    case Indexing(sexprs, Some(lexpr)) =>
      Indexing(sexprs.map(apply), Some(apply(lexpr)))
    case Indexing(sexprs, _)           => Indexing(sexprs.map(apply))
  }

  override protected def apply(member: Member): Member = member match {
    case ExpressionMember(expr) => ExpressionMember(apply(expr))
    case MultiMember(members)   => MultiMember(members.map(apply))
  }

  /////////////////////
  /// LogicalExpression

  override protected def apply(lexpr: LogicalExpression): LogicalExpression = lexpr match {
    case logical.Inclusion.member(m, s) => logical.Inclusion
      .member(apply(m), apply(s))
    case logical.Exclusion.member(m, s) => logical.Inclusion
      .member(apply(m), apply(s))

    case logical.Inclusion.subset(m, s) => logical.Inclusion
      .subset(apply(m), apply(s))
    case logical.Exclusion.subset(m, s) => logical.Inclusion
      .subset(apply(m), apply(s))

    case Logical.and(l1, l2) => Logical.and(apply(l1), apply(l2))
    case Logical.or(l1, l2)  => Logical.or(apply(l1), apply(l2))
    case Logical.not(l)      => Logical.not(apply(l))

    case Comparision.!=(e1, e2) => Comparision.!=(apply(e1), apply(e2))
    case Comparision.==(e1, e2) => Comparision.==(apply(e1), apply(e2))
    case Comparision.<(e1, e2)  => Comparision.<(apply(e1), apply(e2))
    case Comparision.<=(e1, e2) => Comparision.<=(apply(e1), apply(e2))
    case Comparision.>(e1, e2)  => Comparision.>(apply(e1), apply(e2))
    case Comparision.>=(e1, e2) => Comparision.>=(apply(e1), apply(e2))

    case Forall(index, l) => Forall(apply(index), apply(l))
    case Exists(index, l) => Exists(apply(index), apply(l))

  }

  ////////////////
  /// Constraints

  override protected def apply(bounded: BoundedConstraint): BoundedConstraint = bounded match {
    case BoundedConstraint(Some(left), expr, Some(right)) =>
      BoundedConstraint(Some(apply(left)), apply(expr), Some(apply(right)))
    case BoundedConstraint(left, expr, Some(right))       =>
      BoundedConstraint(left, apply(expr), Some(apply(right)))
    case BoundedConstraint(Some(left), expr, right)       =>
      BoundedConstraint(Some(apply(left)), apply(expr), right)
    case BoundedConstraint(left, expr, right)             =>
      BoundedConstraint(left, apply(expr), right)
  }

  override protected def apply(constraint: ConstraintExpression): ConstraintExpression = constraint match {
    case MixedComplementarity(expr, cexpr) =>
      MixedComplementarity(apply(expr), apply(cexpr))


  }

  override protected def apply(cc: ConstraintComparison): ConstraintComparison = cc match {
    case Constraint.<=(expr)  => Constraint.<=(apply(expr))
    case Constraint.===(expr) => Constraint.===(apply(expr))
    case Constraint.>=(expr)  => Constraint.>=(apply(expr))
  }

  /////////////////
  /// DataAttribute

  override protected def apply(attr: Attribute): Attribute = attr match {
    case Relation(name, expr)                       => Relation(name, expr)
    case Inclusion(sexpr)                           => Inclusion(apply(sexpr))
    case DefaultValue(expr)                         => DefaultValue(apply(expr))
    case Defined(expr)                              => Defined(apply(expr))
    case Within(sexpr)                              => Within(apply(sexpr))
    case FinalValue(expr)                           => FinalValue(apply(expr))
    case FinalSet(sexpr)                            => FinalSet(apply(sexpr))
    case DefaultSet(sexpr)                          => DefaultSet(apply(sexpr))
    case Coefficient(Some(index), constraint, expr) => Coefficient(Some(apply(index)), apply(constraint), apply(expr))
    case Coefficient(_, constraint, expr)           => Coefficient(None, apply(constraint), apply(expr))
    case Cover(Some(index), constraint)             => Cover(Some(apply(index)), apply(constraint))
    case Cover(_, constraint)                       => Cover(None, apply(constraint))
    case Objective(Some(index), objective, expr)    => Objective(Some(apply(index)), apply(objective), apply(expr))
    case Objective(_, objective, expr)              => Objective(None, apply(objective), apply(expr))
  }

  //////////////
  /// Reference

  override protected def apply(ref: Reference): Reference = ref

}
