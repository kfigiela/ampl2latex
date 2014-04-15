package pl.edu.agh.mplt.parser.declaration.variable

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.declaration.{VariableAttribute, Attribute}
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.reference.Reference

trait VariableAttributesAMPLParser extends JavaTokenParsers {
  def sexpr: Parser[SetExpression]

  def expr: Parser[Expression]

  def indexing: Parser[Indexing]

  def reference: Parser[Reference]

  def variableAttribute: Parser[VariableAttribute] =
    "binary" ^^ { case _ => Attribute.Binary } |
    "integer" ^^ { case _ => Attribute.Integer } |
    "symbolic" ^^ { case _ => Attribute.Symbolic } |
    (">=" | "<=") ~ expr ^^ { case op ~ expr => Attribute.Relation(op, expr) } |
    "=" ~> expr ^^ Attribute.Defined |
    ":=" ~> expr ^^ Attribute.Initial |
    "default" ~> expr ^^ Attribute.Default |
    "in" ~> sexpr ^^ Attribute.Inclusion |
    "coeff" ~> (indexing ?) ~ reference ~ expr ^^ { case optIndexing ~ constraint ~ expr =>
      Attribute.Coefficient(optIndexing, constraint, expr)
    } |
    "cover" ~> (indexing ?) ~ reference ^^ { case optIndexing ~ constraint =>
      Attribute.Cover(optIndexing, constraint)
    } |
    "obj" ~> (indexing ?) ~ reference ~ expr ^^ { case optIndexing ~ constraint ~ expr =>
      Attribute.Objective(optIndexing, constraint, expr)
    }

}
