package pl.edu.agh.mplt.parser.phrase.set

import pl.edu.agh.mplt.parser.phrase.expression.{Expression, Number}
import pl.edu.agh.mplt.parser.member.SetMember

trait SetLiteral extends SetExpression

case class ExplicitSet[A <: SetMember](members: Set[A] = Set[A]()) extends SetLiteral

case class SetComprehension(start: SetMember,
                            end: SetMember,
                            step: Expression = Number("1")) extends SetLiteral


