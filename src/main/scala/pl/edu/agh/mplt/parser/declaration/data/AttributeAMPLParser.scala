package pl.edu.agh.mplt.parser.declaration.data

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.phrase.expression.Expression
import pl.edu.agh.mplt.parser.phrase.set.{Indexing, SetExpression}
import pl.edu.agh.mplt.parser.reference.Reference
import language.postfixOps

trait AttributeAMPLParser extends JavaTokenParsers {
   def expr: Parser[Expression]

   def sexpr: Parser[SetExpression]

   def indexing: Parser[Indexing]

   def reference: Parser[Reference]

   def attribute: Parser[Attribute] =
      "binary" ^^ { _ => Attribute.Binary } |
      "integer" ^^ { _ => Attribute.Integer } |
      "symbolic" ^^ { _ => Attribute.Symbolic } |
      "in" ~> sexpr ^^ Attribute.Inclusion |
      "dimen" ~> wholeNumber ^^ Attribute.Dimension |
      "within" ~> sexpr ^^ Attribute.Membership |
      (":=" | "=") ~> (sexpr ^^ Attribute.FinalSet | expr ^^ Attribute.FinalValue) |
      "default" ~> (sexpr ^^ Attribute.DefaultSet | expr ^^ Attribute.DefaultValue) |
      "<>" ~ expr ^^ { case "<>" ~ expr => Attribute.Relation("!=", expr) } |
      relationOperators ~ expr ^^ { case op ~ expr => Attribute.Relation(op, expr) } |
      "coeff" ~> (indexing ?) ~ reference ~ expr ^^ {
         case optIndexing ~ constraint ~ expr => Attribute.Coefficient(optIndexing, constraint, expr)
      } |
      "cover" ~> (indexing ?) ~ reference ^^ {
         case optIndexing ~ constraint => Attribute.Cover(optIndexing, constraint)
      } |
      "obj" ~> (indexing ?) ~ reference ~ expr ^^ {
         case optIndexing ~ constraint ~ expr => Attribute.Objective(optIndexing, constraint, expr)
      }


   private def relationOperators = List[Parser[String]]("<=", "<", "==", "!=", ">=", ">")
                                   .reduce(_ | _)
}
