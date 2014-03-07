package pl.edu.agh.mplt.parser.declaration.set.attributes

import pl.edu.agh.mplt.parser.IntercodeExpression
import pl.edu.agh.mplt.parser.expression.set.SetExpression


trait SetAttribute extends IntercodeExpression

object SetAttribute {

  case class dimension(n: String) extends SetAttribute

  case class within(set: SetExpression) extends SetAttribute

  case class is(set: SetExpression) extends SetAttribute

  case class default(set: SetExpression) extends SetAttribute

}