package pl.edu.agh.mplt.parser.set

import pl.edu.agh.mplt.parser.expression.{Number, Expression}

trait SetLiteral extends SetExpression

case class ExplicitSet[A](members: scala.collection.immutable.Set[A] = Set[A]()) extends SetLiteral

case class SetComprehension(start: Expression,
                            end: Expression,
                            step: Expression = Number("1")) extends SetLiteral

case class SetExpressionWithDummyMember(id: String,
                                        sexpr: SetExpression) extends SetExpression