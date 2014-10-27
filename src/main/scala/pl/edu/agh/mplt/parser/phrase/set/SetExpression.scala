package pl.edu.agh.mplt.parser.phrase.set

import pl.edu.agh.mplt.parser.phrase.Phrase
import pl.edu.agh.mplt.parser.phrase.logical.LogicalExpression
import pl.edu.agh.mplt.parser.member.SetMember

trait SetExpression extends Phrase

case class ParenthesizedSetExpression(expr: SetExpression) extends SetExpression


case class SetExpressionIf(lexpr: LogicalExpression, left: SetExpression,
                           right: SetExpression) extends SetExpression


case class Indexing(sexprs: List[SetExpression],
                    lexpr: Option[LogicalExpression] = None) extends SetExpression

case class IndexedSet(indexes: List[String],
                      sexpr: SetExpression) extends SetExpression

object Sets {

   case class SetOf(indexing: Indexing, member: SetMember) extends SetExpression

   case class Union(s1: SetExpression, s2: SetExpression) extends SetExpression

   case class Intersection(s1: SetExpression, s2: SetExpression) extends SetExpression

   case class Difference(s1: SetExpression, s2: SetExpression) extends SetExpression

   case class SymetricDifference(s1: SetExpression, s2: SetExpression) extends SetExpression

   case class Cartesian(s1: SetExpression, s2: SetExpression) extends SetExpression

}