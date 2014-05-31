package pl.edu.agh.mplt.parser.declaration.data

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.phrase.expression.Expression
import pl.edu.agh.mplt.parser.phrase.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.reference.Reference


trait Attribute extends ASTNode

object Attribute {

  case object Binary extends Attribute

  case object Integer extends Attribute

  case object Symbolic extends Attribute

  case class Relation(op: String, expr: Expression) extends Attribute

  case class Inclusion(sexpr: SetExpression) extends Attribute

  case class DefaultValue(expr: Expression) extends Attribute

  case class Definition(expr: Expression) extends Attribute

  case class Dimension(n: String) extends Attribute

  case class Membership(set: SetExpression) extends Attribute

  case class FinalValue(expr: Expression) extends Attribute

  case class FinalSet(set: SetExpression) extends Attribute

  case class DefaultSet(set: SetExpression) extends Attribute

  case class Coefficient(indexing: Option[Indexing], ref: Reference, expr: Expression) extends Attribute

  case class Cover(indexing: Option[Indexing], ref: Reference) extends Attribute

  case class Objective(indexing: Option[Indexing], ref: Reference, expr: Expression) extends Attribute

}