package pl.edu.agh.mplt.parser.declaration.set.attributes

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.formula.set.SetExpression


trait SetAttribute extends ASTNode

object SetAttribute {

  case class dimension(n: String) extends SetAttribute

  case class within(set: SetExpression) extends SetAttribute

  case class is(set: SetExpression) extends SetAttribute

  case class default(set: SetExpression) extends SetAttribute

}
