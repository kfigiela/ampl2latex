package pl.edu.agh.mplt.parser.expression.set

import pl.edu.agh.mplt.parser.expression.{StringLiteral, Expression}

trait SetLiteral extends SetExpression

case class ExplicitSet[A](members: scala.collection.immutable.Set[A] = Set[A]()) extends SetLiteral

case class SetComprehension(start: Expression,
                            end: Expression,
                            step: Expression = StringLiteral("1")) extends SetLiteral
