package pl.edu.agh.mplt.parser.declaration.data

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.reference.Reference


trait Attribute extends ASTNode

object Attribute {

  case object Binary extends Attribute

  case object Integer extends Attribute

  case object Symbolic extends Attribute

  case class Relation(op: String, expr: Expression) extends Attribute

  case class Inclusion(sexpr: SetExpression) extends Attribute

  case class DefaultValue(expr: Expression) extends Attribute

  case class Defined(expr: Expression) extends Attribute

  case class Dimension(n: String) extends Attribute

  case class Within(set: SetExpression) extends Attribute

  case class FinalValue(expr: Expression) extends Attribute

  case class FinalSet(set: SetExpression) extends Attribute

  case class DefaultSet(set: SetExpression) extends Attribute

  case class Coefficient(indexing: Option[Indexing], constraint: Reference, expr: Expression) extends Attribute

  case class Cover(indexing: Option[Indexing], constraint: Reference) extends Attribute

  case class Objective(indexing: Option[Indexing], objective: Reference, expr: Expression) extends Attribute

}