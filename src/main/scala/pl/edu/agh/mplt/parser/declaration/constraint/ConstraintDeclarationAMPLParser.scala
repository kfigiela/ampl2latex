package pl.edu.agh.mplt.parser.declaration.constraint

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.phrase.set.Indexing


trait ConstraintDeclarationAMPLParser extends JavaTokenParsers {
   def nonKeyword: Parser[String]

   def dequotedLiteral: Parser[String]

   def indexing: Parser[Indexing]

   def constraintExpression: Parser[ConstraintExpression]

   def constraintDeclaration: Parser[ConstraintDeclaration] =
      ("subject to" ?) ~> nonKeyword ~ (dequotedLiteral ?) ~ (indexing ?) ~ (":" ~> constraintExpression <~ ";") ^? {
         case name ~ optAlias ~ optIndexing ~ constraint => ConstraintDeclaration(name, optAlias, optIndexing, constraint)
      }
}
