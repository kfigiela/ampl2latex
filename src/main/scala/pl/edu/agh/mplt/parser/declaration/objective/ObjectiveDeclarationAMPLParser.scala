package pl.edu.agh.mplt.parser.declaration.objective

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.phrase.set.Indexing
import pl.edu.agh.mplt.parser.phrase.expression.Expression
import language.postfixOps

trait ObjectiveDeclarationAMPLParser extends JavaTokenParsers {
   def nonKeyword: Parser[String]

   def objective: Parser[String] = "maximize" | "minimize"

   def indexing: Parser[Indexing]

   def expr: Parser[Expression]

   def dequotedLiteral: Parser[String]

   def objectiveDeclaration: Parser[ObjectiveDeclaration] =
      objective ~ nonKeyword ~ (dequotedLiteral ?) ~ (indexing ?) ~ (":" ~> expr ?) <~ ";" ^^ {
         case "maximize" ~ name ~ optAlias ~ optIndexing ~ (exprOpt) => Maximize(name, optAlias, optIndexing,
            exprOpt)
         case "minimize" ~ name ~ optAlias ~ optIndexing ~ (exprOpt) => Minimize(name, optAlias, optIndexing,
            exprOpt)
      }


}
