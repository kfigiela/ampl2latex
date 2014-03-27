package pl.edu.agh.mplt.parser.declaration.set

import pl.edu.agh.mplt.parser.formula.set.SetExpression
import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.declaration.{Attribute, SetAttribute}

trait SetAttributesAMPLParser extends JavaTokenParsers {
  def sexpr: Parser[SetExpression]

  def setAttributes: Parser[List[SetAttribute]] = rep(attribute)

  def attribute: Parser[SetAttribute] = dimension | attrWithin | eq | default

  private def dimension = "dimen" ~> "[+-]?\\d+".r ^^ Attribute.Dimension

  private def attrWithin = "within" ~> sexpr ^^ Attribute.Within

  private def eq = "=" ~> sexpr ^^ Attribute.InitialSet

  private def default = "default" ~> sexpr ^^ Attribute.DefaultSet

}
