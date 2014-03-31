package pl.edu.agh.mplt.parser.declaration.objective

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.expression.Expression

trait ObjectiveDeclarationAMPLParser extends JavaTokenParsers {
  def nonKeyword: Parser[String]

  def indexing: Parser[Indexing]

  def expr: Parser[Expression]

  def objectiveDeclaration: Parser[ObjectiveDeclaration] =
    ("maximize" | "minimize") ~ nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ ":" ~ expr <~ ";" ^^ {
      case "maximize" ~ name ~ optAlias ~ optIndexing ~ _ ~ expr => Maximize(name, optAlias, optIndexing, expr)
      case "minimize" ~ name ~ optAlias ~ optIndexing ~ _ ~ expr => Minimize(name, optAlias, optIndexing, expr)
    }


}
