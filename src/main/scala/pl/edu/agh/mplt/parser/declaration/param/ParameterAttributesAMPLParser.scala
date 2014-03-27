package pl.edu.agh.mplt.parser.declaration.param

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.SetExpression
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.declaration.{ParameterAttribute, Attribute}

trait ParameterAttributesAMPLParser extends JavaTokenParsers {
  def sexpr: Parser[SetExpression]

  def expr: Parser[Expression]

  def parameterAttribute: Parser[ParameterAttribute] =
    "binary" ^^ { case _ => Attribute.Binary} |
      "integer" ^^ { case _ => Attribute.Integer} |
      "symbolic" ^^ { case _ => Attribute.Symbolic} |
      relationOperators ~ expr ^^ { case op ~ expr => Attribute.Relation(op, expr)} |
      "=" ~> expr ^^ { case expr => Attribute.Relation("==", expr)} |
      "<>" ~> expr ^^ { case expr => Attribute.Relation("!=", expr)} |
      "default" ~> expr ^^ Attribute.Default |
      "in" ~> sexpr ^^ Attribute.Inclusion

  private def relationOperators = List[Parser[String]]("<=", "<", "==", "!=", ">=", ">").reduce(_ | _)
}