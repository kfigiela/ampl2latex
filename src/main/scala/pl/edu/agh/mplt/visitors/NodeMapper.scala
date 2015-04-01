package pl.edu.agh.mplt.visitors


import scala.collection.mutable

import pl.edu.agh.mplt.parser.declaration.{InvalidDeclaration, Declaration}
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.declaration.constraint._
import pl.edu.agh.mplt.parser.declaration.data._
import pl.edu.agh.mplt.parser.declaration.data.Attribute._
import pl.edu.agh.mplt.parser.declaration.objective._
import pl.edu.agh.mplt.parser.phrase.logical
import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.phrase.expression.Bin._
import pl.edu.agh.mplt.parser.phrase.expression.ExpressionReduction._
import pl.edu.agh.mplt.parser.phrase.logical._
import pl.edu.agh.mplt.parser.phrase.logical.LogicalReduction._
import pl.edu.agh.mplt.parser.phrase.set._
import pl.edu.agh.mplt.parser.phrase.set.Sets._
import pl.edu.agh.mplt.parser.member._
import pl.edu.agh.mplt.parser.reference._

class NodeMapper(val operations: mutable.Buffer[NodeMapper]) {
   def apply(node: Declaration): Declaration = {
      (operations :\ map(node))((f, n) => f(n))
   }

   def andThen(mapping: NodeMapper): NodeMapper = {
      mapping +=: operations
      this
   }

   def andThen[B](v: Visitor[Declaration, B]): NodeAggregator[B] = new
   NodeAggregator[B](operations :+ this, v)

   def map(node: Declaration): Declaration = node match {
      case Assertion(index, lexpr) => Assertion(index.map(mapIndexing), mapLexpr(lexpr))

      case ConstraintDeclaration(n, a, index, constr) =>
         ConstraintDeclaration(mapName(n), mapAlias(a),
            index.map(mapIndexing), mapConstraint(constr))

      case Minimize(n, a, index, expr) =>
         Minimize(mapName(n), mapAlias(a),
            index.map(mapIndexing), expr.map(mapExpr))

      case Maximize(n, a, index, expr) =>
         Maximize(mapName(n), mapAlias(a),
            index.map(mapIndexing), expr.map(mapExpr))

      case ParameterDeclaration(n, a, index, attrs) =>
         ParameterDeclaration(mapName(n), mapAlias(a),
            index.map(mapIndexing), attrs.map(mapAttribute))


      case VariableDeclaration(n, a, index, attrs) =>
         VariableDeclaration(mapName(n), mapAlias(a),
            index.map(mapIndexing), attrs.map(mapAttribute))

      case SetDeclaration(n, a, index, attrs) =>
         SetDeclaration(mapName(n), mapAlias(a),
            index.map(mapIndexing), attrs.map(mapAttribute))

      case error@InvalidDeclaration(_) => error

      case n => throw new Error(s"Usupported: $n")
   }

   def mapName(name: String): String = name

   def mapAlias(aliaOpt: Option[String]): Option[String] = aliaOpt

   def mapIndexes(indexes: List[String]): List[String] = indexes

   def mapNumber(n: Number): Number = n

   //////////////
   /// Expression

   def mapExpr(e: Expression): Expression = e match {
      case ParenthesizedExpression(expr) =>
         ParenthesizedExpression(mapExpr(expr))

      case ExpressionIf(cond, t, f) =>
         ExpressionIf(mapLexpr(cond), mapExpr(t), mapExpr(f))

      case FunctionCall(name, args) =>
         FunctionCall(mapName(name), args.map(mapExpr))

      case PiecewiseLinearTerm(optExpres, optIndexes, (expr, cexpr)) =>
         PiecewiseLinearTerm(optExpres.map(b =>
            (b._1.map(mapIndexing), mapExpr(b._2))),
            optIndexes.map(s =>
               (s._1.map(mapIndexing), mapExpr(s._2))),
            (mapExpr(expr), cexpr.map(mapExpr)))

      case Unary.-(expr) => Unary.-(mapExpr(expr))

      case n@Number(_) => mapNumber(n)

      case bin: BinaryOperation           => mapBinary(bin)
      case reduction: ExpressionReduction => mapReduction(reduction)

      case ref: Reference => mapReference(ref)

      case node => throw new Error(s"Usupported: $node")
   }

   def mapBinary(bin: BinaryOperation): BinaryOperation = bin match {
      case +(left, right)    => Bin.+(mapExpr(left), mapExpr(right))
      case -(left, right)    => Bin.-(mapExpr(left), mapExpr(right))
      case Less(left, right) => Less(mapExpr(left), mapExpr(right))
      case *(left, right)    => *(mapExpr(left), mapExpr(right))
      case /(left, right)    => /(mapExpr(left), mapExpr(right))
      case Div(left, right)  => Div(mapExpr(left), mapExpr(right))
      case Mod(left, right)  => Mod(mapExpr(left), mapExpr(right))
      case ^(left, right)    => ^(mapExpr(left), mapExpr(right))

      case node => throw new Error(s"Usupported: $node")
   }

   def mapReduction(red: ExpressionReduction): ExpressionReduction = red match {
      case Sum(indexing, expr)  => Sum(mapIndexing(indexing), mapExpr(expr))
      case Prod(indexing, expr) => Prod(mapIndexing(indexing), mapExpr(expr))
      case Max(indexing, expr)  => Max(mapIndexing(indexing), mapExpr(expr))
      case Min(indexing, expr)  => Min(mapIndexing(indexing), mapExpr(expr))

      case node => throw new Error(s"Usupported: $node")
   }

   /////////////////
   /// SetExpression

   def mapSexpr(s: SetExpression): SetExpression = s match {
      case ParenthesizedSetExpression(sexpr) =>
         ParenthesizedSetExpression(mapSexpr(sexpr))

      case SetExpressionIf(cond, t, f) =>
         SetExpressionIf(mapLexpr(cond), mapSexpr(t), mapSexpr(f))
      case IndexedSet(is, sexpr)       =>
         IndexedSet(mapIndexes(is), mapSexpr(sexpr))

      case SymetricDifference(s1, s2) => SymetricDifference(mapSexpr(s1), mapSexpr(s2))
      case Intersection(s1, s2)       => Intersection(mapSexpr(s1), mapSexpr(s2))
      case Difference(s1, s2)         => Difference(mapSexpr(s1), mapSexpr(s2))
      case Cartesian(s1, s2)          => Cartesian(mapSexpr(s1), mapSexpr(s2))
      case SetOf(index, m)            => SetOf(mapIndexing(index), mapMember(m))
      case Union(s1, s2)              => Union(mapSexpr(s1), mapSexpr(s2))

      case ExplicitSet(members)               => ExplicitSet(members.map(mapMember))
      case SetComprehension(start, end, step) => SetComprehension(mapMember(start), mapMember(end), mapExpr(step))

      case ref: Reference => mapReference(ref)

      case node => throw new Error(s"Usupported: $node")
   }

   ////////////////
   /// Indexing

   def mapIndexing(index: Indexing): Indexing = index match {
      case Indexing(sexprs, lexpr) =>
         Indexing(sexprs.map(mapSexpr), lexpr.map(mapLexpr))

      case node => throw new Error(s"Usupported: $node")
   }

   def mapMember(member: SetMember): SetMember = member match {
      case ExpressionMember(expr) => ExpressionMember(mapExpr(expr))
      case MultiMember(members)   => MultiMember(members.map(mapMember))

      case node => throw new Error(s"Usupported: $node")
   }

   /////////////////////
   /// LogicalExpression

   def mapLexpr(lexpr: LogicalExpression): LogicalExpression = lexpr match {
      case ParenthesizedLogical(l) => l

      case logical.Inclusion.Member(m, s) =>
         logical.Inclusion.Member(mapMember(m), mapSexpr(s))
      case logical.Exclusion.Member(m, s) =>
         logical.Inclusion.Member(mapMember(m), mapSexpr(s))

      case logical.Inclusion.Subset(m, s) =>
         logical.Inclusion.Subset(mapSexpr(m), mapSexpr(s))
      case logical.Exclusion.Subset(m, s) =>
         logical.Inclusion.Subset(mapSexpr(m), mapSexpr(s))

      case Logical.And(l1, l2) => Logical.And(mapLexpr(l1), mapLexpr(l2))
      case Logical.Or(l1, l2)  => Logical.Or(mapLexpr(l1), mapLexpr(l2))
      case Logical.Not(l)      => Logical.Not(mapLexpr(l))

      case Comparision.!=(e1, e2) => Comparision.!=(mapExpr(e1), mapExpr(e2))
      case Comparision.==(e1, e2) => Comparision.==(mapExpr(e1), mapExpr(e2))
      case Comparision.<(e1, e2)  => Comparision.<(mapExpr(e1), mapExpr(e2))
      case Comparision.<=(e1, e2) => Comparision.<=(mapExpr(e1), mapExpr(e2))
      case Comparision.>(e1, e2)  => Comparision.>(mapExpr(e1), mapExpr(e2))
      case Comparision.>=(e1, e2) => Comparision.>=(mapExpr(e1), mapExpr(e2))

      case Forall(index, l) => Forall(mapIndexing(index), mapLexpr(l))
      case Exists(index, l) => Exists(mapIndexing(index), mapLexpr(l))

      case ref: Reference => mapReference(ref)

      case node => throw new Error(s"Usupported: $node")
   }

   ////////////////
   /// Constraints

   def mapBoundedConstraint(bounded: BoundedConstraint): BoundedConstraint = bounded match {
      case BoundedConstraint(left, expr, right) =>
         BoundedConstraint(left.map(mapConstraintComparision),
            mapExpr(expr), right.map(mapConstraintComparision))

      case node => throw new Error(s"Usupported: $node")
   }

   def mapConstraint(constraint: ConstraintExpression): ConstraintExpression = constraint match {
      case BoundedConstraint(left, expr, right) =>
         BoundedConstraint(left.map(mapConstraintComparision), mapExpr(expr), right.map(mapConstraintComparision))

      case MixedComplementarity(expr, cexpr) =>
         MixedComplementarity(mapExpr(expr), mapBoundedConstraint(cexpr))

      case node => throw new Error(s"Usupported: $node")
   }

   def mapConstraintComparision(cc: ConstraintComparison): ConstraintComparison = cc match {
      case Constraint.<=(expr) => Constraint.<=(mapExpr(expr))
      case Constraint.==(expr) => Constraint.==(mapExpr(expr))
      case Constraint.>=(expr) => Constraint.>=(mapExpr(expr))

      case node => throw new Error(s"Usupported: $node")
   }

   /////////////////
   /// DataAttribute

   def mapAttribute(attr: Attribute): Attribute = attr match {
      case Binary                     => Binary  
      case Integer                    => Integer 
      case Symbolic                   => Symbolic
      case d@Dimension(_)             => d
      case Relation(name, expr)       => Relation(mapName(name), mapExpr(expr))
      case DefaultValue(expr)         => DefaultValue(mapExpr(expr))
      case Definition(expr)           => Definition(mapExpr(expr))
      case FinalValue(expr)           => FinalValue(mapExpr(expr))
      case Attribute.Inclusion(sexpr) => Attribute.Inclusion(mapSexpr(sexpr))
      case Membership(sexpr)          => Membership(mapSexpr(sexpr))
      case FinalSet(sexpr)            => FinalSet(mapSexpr(sexpr))
      case DefaultSet(sexpr)          => DefaultSet(mapSexpr(sexpr))

      case Cover(index, ref)             =>
         Cover(index.map(mapIndexing), mapReference(ref))
      case Objective(index, ref, expr)   =>
         Objective(index.map(mapIndexing), mapReference(ref), mapExpr(expr))
      case Coefficient(index, ref, expr) =>
         Coefficient(index.map(mapIndexing), mapReference(ref), mapExpr(expr))

      case node => throw new Error(s"Usupported: $node")
   }

   //////////////
   /// Reference

   def mapReference(ref: Reference): Reference = ref match {
      case IndexedReference(r, exprs) => IndexedReference(mapReference(r), exprs.map(mapExpr))
      case SubIndexedReference(r, exprs) => SubIndexedReference(mapReference(r), exprs)
      case SimpleReference(name)      => SimpleReference(mapName(name))
      case SymbolicReference(name)    => SymbolicReference((name))

      case node => throw new Error(s"Usupported: $node")
   }
}
