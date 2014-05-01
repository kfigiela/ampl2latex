package pl.edu.agh.mplt.parser.formula.set

import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression

trait SetLiteral extends SetExpression

case class ExplicitSet[A <: Member](members: Set[A] = Set[A]()) extends SetLiteral

case class SetComprehension(start: Member,
                            end: Member,
                            step: Number = Number("1")) extends SetLiteral


