package pl.edu.agh.mplt.parser.formula.set

import pl.edu.agh.mplt.parser.formula.Formula

trait SetExpression extends Formula

object Sets {

  case class Union(s1: SetExpression, s2: SetExpression) extends SetExpression

  case class Intersection(s1: SetExpression, s2: SetExpression) extends SetExpression

  case class Difference(s1: SetExpression, s2: SetExpression) extends SetExpression

  case class SymetricDifference(s1: SetExpression, s2: SetExpression) extends SetExpression

  case class Cartesian(s1: SetExpression, s2: SetExpression) extends SetExpression

}