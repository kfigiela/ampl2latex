package pl.edu.agh.mplt.parser.declaration.objective

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.expression.Expression
import language.postfixOps
trait ObjectiveDeclarationAMPLParser extends JavaTokenParsers {
  def nonKeyword: Parser[String]

  def objective: Parser[String] = "maximize" | "minimize"

  def indexing: Parser[Indexing]

  def expr: Parser[Expression]

  def objectiveDeclaration: Parser[ObjectiveDeclaration] =
    objective ~ nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ (":" ~> expr ?) <~ ";" ^^ {
      case "maximize" ~ name ~ optAlias ~ optIndexing ~ (exprOpt) => Maximize(name, optAlias, optIndexing,
        exprOpt)
      case "minimize" ~ name ~ optAlias ~ optIndexing ~ (exprOpt) => Minimize(name, optAlias, optIndexing,
        exprOpt)
    }


}
