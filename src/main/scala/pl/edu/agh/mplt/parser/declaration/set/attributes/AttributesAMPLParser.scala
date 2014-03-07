package pl.edu.agh.mplt.parser.declaration.set.attributes

import pl.edu.agh.mplt.parser.expression.set.SetExpression
import scala.util.parsing.combinator.JavaTokenParsers

trait AttributesAMPLParser extends JavaTokenParsers {
  def sexpr: Parser[SetExpression]

  def attributes: Parser[List[SetAttribute]] = rep(attribute)

  private def attribute: Parser[SetAttribute] = dimension | within | eq | default

  private def dimension = "dimen" ~> "[+-]?\\d+".r ^^ SetAttribute.dimension

  private def within = "within" ~> sexpr ^^ SetAttribute.within

  private def eq = "=" ~> sexpr ^^ SetAttribute.is

  private def default = "default" ~> sexpr ^^ SetAttribute.default

}
