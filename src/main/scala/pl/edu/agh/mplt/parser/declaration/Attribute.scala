package pl.edu.agh.mplt.parser.declaration

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.SetExpression


trait ParameterAttribute extends ASTNode

trait VariableAttribute extends ASTNode

trait SetAttribute extends ASTNode

object Attribute {

  case object Binary extends ParameterAttribute with VariableAttribute

  case object Integer extends ParameterAttribute with VariableAttribute

  case object Symbolic extends ParameterAttribute with VariableAttribute

  case class Relation(op: String, expr: Expression) extends ParameterAttribute with VariableAttribute

  case class Inclusion(sexpr: SetExpression) extends ParameterAttribute with VariableAttribute

  case class Default(expr: Expression) extends ParameterAttribute with VariableAttribute

  case class Initial(expr: Expression) extends VariableAttribute

  case class Dimension(n: String) extends SetAttribute

  case class Within(set: SetExpression) extends SetAttribute

  case class InitialSet(set: SetExpression) extends SetAttribute

  case class DefaultSet(set: SetExpression) extends SetAttribute

}