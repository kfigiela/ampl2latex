package pl.edu.agh.mplt.parser.declaration.data

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.phrase.set.Indexing
import language.postfixOps

trait DatatypeDeclarationAMPLParser extends JavaTokenParsers {
   def indexing: Parser[Indexing]

   def attribute: Parser[Attribute]

   def nonKeyword: Parser[String]

   def nonAttributeKeyword: Parser[String]

   private def datatype: Parser[String] = "param" | "set" | "var"

   def datatypeDeclaration: Parser[DataDeclaration] =
      datatype ~ nonKeyword ~ (nonAttributeKeyword ?) ~ (indexing ?) ~ repsep(attribute, "," ?) <~ ";" ^^ {
         case "param" ~ name ~ optAlias ~ optIndexing ~ optAttributes => ParameterDeclaration(name, optAlias, optIndexing,
            optAttributes)
         case "set" ~ name ~ optAlias ~ optIndexing ~ optAttributes   => SetDeclaration(name, optAlias, optIndexing,
            optAttributes)
         case "var" ~ name ~ optAlias ~ optIndexing ~ optAttributes   => VariableDeclaration(name, optAlias, optIndexing,
            optAttributes)
      }

}
