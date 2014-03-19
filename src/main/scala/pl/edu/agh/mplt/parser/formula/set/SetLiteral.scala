package pl.edu.agh.mplt.parser.formula.set

import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression

trait SetLiteral extends SetExpression

case class ExplicitSet[A <: Member](members: Set[A] = Set[A]()) extends SetLiteral

case class SetComprehension(start: Member,
                            end: Member,
                            step: Number = Number("1")) extends SetLiteral

case class SetExpressionWithDummyMember(id: String,
                                        sexpr: SetExpression) extends SetExpression

case class Indexing(sexprs: List[SetExpression],
                    lexpr: Option[LogicalExpression] = None) extends SetExpression

case class IndexedSet(indexes: List[String],
                      sexpr: SetExpression) extends SetExpression