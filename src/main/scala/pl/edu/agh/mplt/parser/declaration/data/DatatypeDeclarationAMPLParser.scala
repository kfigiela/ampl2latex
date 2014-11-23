package pl.edu.agh.mplt.parser.declaration.data

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.phrase.set.Indexing
import language.postfixOps

trait DatatypeDeclarationAMPLParser extends JavaTokenParsers {
   def indexing: Parser[Indexing]

   def setAttribute: Parser[Attribute]

   def paramAttribute: Parser[Attribute]

   def varAttribute: Parser[Attribute]

   def nonKeyword: Parser[String]

   def nonAttributeKeyword: Parser[String]


   private[this] def common = whiteSpace ~ nonKeyword ~ (nonAttributeKeyword ?) ~ (indexing ?)

   def datatypeDeclaration: Parser[DataDeclaration] =
      "param" ~> common ~ repsep(paramAttribute, "," ?) <~ ";" ^^ {
         case name ~ optAlias ~ optIndexing ~ optAttributes => ParameterDeclaration(name, optAlias, optIndexing,
            optAttributes)
      } | "set" ~> common ~ repsep(setAttribute, "," ?) <~ ";" ^^ {
         case name ~ optAlias ~ optIndexing ~ optAttributes => SetDeclaration(name, optAlias, optIndexing,
            optAttributes)
      } | "var" ~> common ~ repsep(varAttribute, "," ?) <~ ";" ^^ {
         case name ~ optAlias ~ optIndexing ~ optAttributes => VariableDeclaration(name, optAlias, optIndexing,
            optAttributes)
      }

}
