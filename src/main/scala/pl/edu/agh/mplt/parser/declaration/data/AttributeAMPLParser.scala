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

   def setAttribute: Parser[Attribute] =
      (":=" | "=") ~> sexpr ^^ Attribute.FinalSet |
      "dimen" ~> wholeNumber ^^ Attribute.Dimension |
      "within" ~> sexpr ^^ Attribute.Membership |
      "default" ~> sexpr ^^ Attribute.DefaultSet

   def paramAttribute: Parser[Attribute] =
      common |
      relationOperators ~ expr ^^ { case op ~ expr => Attribute.Relation(op, expr) }


   def varAttribute: Parser[Attribute] =
      common |
      ("<=" | ">=") ~ expr ^^ { case op ~ expr => Attribute.Relation(op, expr) } |
      "coeff" ~> (indexing ?) ~ reference ~ expr ^^ {
         case optIndexing ~ constraint ~ expr => Attribute.Coefficient(optIndexing, constraint, expr)
      } |
      "cover" ~> (indexing ?) ~ reference ^^ {
         case optIndexing ~ constraint => Attribute.Cover(optIndexing, constraint)
      } |
      "obj" ~> (indexing ?) ~ reference ~ expr ^^ {
         case optIndexing ~ constraint ~ expr => Attribute.Objective(optIndexing, constraint, expr)
      }

   private[this] def common: Parser[Attribute] =
      "binary" ^^ { _ => Attribute.Binary } |
      "integer" ^^ { _ => Attribute.Integer } |
      "symbolic" ^^ { _ => Attribute.Symbolic } |
      (":=" | "=") ~> expr ^^ Attribute.FinalValue |
      "default" ~> expr ^^ Attribute.DefaultValue |
      "in" ~> sexpr ^^ Attribute.Inclusion


   private def relationOperators = List[Parser[String]]("<=", "<", "==", "!=", ">=", ">")
   .reduce(_ | _)
}
