package pl.edu.agh.mplt.parser.declaration.set

import pl.edu.agh.mplt.parser.formula.set.SetExpression
import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.declaration.{Attribute, SetAttribute}

trait SetAttributesAMPLParser extends JavaTokenParsers {
  def sexpr: Parser[SetExpression]

  def setAttribute: Parser[SetAttribute] =
    "dimen" ~> wholeNumber ^^ Attribute.Dimension |
      "within" ~> sexpr ^^ Attribute.Within |
      "=" ~> sexpr ^^ Attribute.InitialSet |
      "default" ~> sexpr ^^ Attribute.DefaultSet

}
