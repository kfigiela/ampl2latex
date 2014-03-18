package pl.edu.agh.mplt.parser.declaration.set.attributes

import pl.edu.agh.mplt.parser.set.SetExpression
import scala.util.parsing.combinator.JavaTokenParsers

trait AttributesAMPLParser extends JavaTokenParsers {
  def sexpr: Parser[SetExpression]

  def attributes: Parser[List[SetAttribute]] = rep(attribute)

  def attribute: Parser[SetAttribute] = dimension | attrWithin | eq | default

  private def dimension = "dimen" ~> "[+-]?\\d+".r ^^ SetAttribute.dimension

  private def attrWithin = "within" ~> sexpr ^^ SetAttribute.within

  private def eq = "=" ~> sexpr ^^ SetAttribute.is

  private def default = "default" ~> sexpr ^^ SetAttribute.default

}
