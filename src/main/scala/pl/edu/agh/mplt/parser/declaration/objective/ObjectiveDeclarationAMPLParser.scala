package pl.edu.agh.mplt.parser.declaration.objective

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.declaration.PiecewiseLinearTerm

trait ObjectiveDeclarationAMPLParser extends JavaTokenParsers {
  def nonKeyword: Parser[String]

  def keyword: Parser[String]

  def indexing: Parser[Indexing]

  def expr: Parser[Expression]

  def piecewiseLinearTerm: Parser[PiecewiseLinearTerm]

  def objectiveDeclaration: Parser[ObjectiveDeclaration] =
    keyword ~ nonKeyword ~ (nonKeyword ?) ~ (indexing ?) ~ ((":" ~> expr ?) ~ (piecewiseLinearTerm ?)) <~ ";" ^^ {
      case "maximize" ~ name ~ optAlias ~ optIndexing ~ (exprOpt ~ piecewiseOpt) => Maximize(name, optAlias, optIndexing,
        exprOpt, piecewiseOpt)
      case "minimize" ~ name ~ optAlias ~ optIndexing ~ (exprOpt ~ piecewiseOpt) => Minimize(name, optAlias, optIndexing,
        exprOpt, piecewiseOpt)
    }


}
