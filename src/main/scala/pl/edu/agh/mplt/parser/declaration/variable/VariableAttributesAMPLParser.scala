package pl.edu.agh.mplt.parser.declaration.variable

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.SetExpression
import pl.edu.agh.mplt.parser.declaration.{VariableAttribute, Attribute}
import pl.edu.agh.mplt.parser.formula.expression.Expression

trait VariableAttributesAMPLParser extends JavaTokenParsers {
  def sexpr: Parser[SetExpression]

  def expr: Parser[Expression]

  def variableAttribute: Parser[VariableAttribute] =
    "binary" ^^ { case _ => Attribute.Binary} |
      "integer" ^^ { case _ => Attribute.Integer} |
      "symbolic" ^^ { case _ => Attribute.Symbolic} |
      (">=" | "<=" | "=" ^^ { case _ => "=="}) ~ expr ^^ { case op ~ expr => Attribute.Relation(op, expr)} |
      ":=" ~> expr ^^ Attribute.Initial |
      "default" ~> expr ^^ Attribute.Default |
      "in" ~> sexpr ^^ Attribute.Inclusion
}
