package pl.edu.agh.mplt.parser.set

import pl.edu.agh.mplt.parser.logical.expression.Number
import pl.edu.agh.mplt.parser.member.Member

trait SetLiteral extends SetExpression

case class ExplicitSet[A <: Member](members: Set[A] = Set[A]()) extends SetLiteral

case class SetComprehension(start: Member,
                            end: Member,
                            step: Number = Number("1")) extends SetLiteral

case class SetExpressionWithDummyMember(id: String,
                                        sexpr: SetExpression) extends SetExpression