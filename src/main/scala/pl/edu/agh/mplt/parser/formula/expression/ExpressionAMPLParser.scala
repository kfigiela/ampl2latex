package pl.edu.agh.mplt.parser.formula.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.parser.formula.set.Indexing

trait ExpressionAMPLParser extends JavaTokenParsers {

  def reference: Parser[Reference]

  def indexing: Parser[Indexing]

  def expr: Parser[Expression] = arithmeticExpression | freeTokens

  def number: Parser[Number] = """-?(\d+(\.\d+)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ Number

  def keyword: Parser[String]

  private def arithmeticExpression: Parser[Expression] =
    chainl1(production1, "+" ^^^ Bin.+ | "-" ^^^ Bin.- | "less" ^^^ Bin.less)

  private def production1 = chainl1(production2, "*" ^^^ Bin.* | "/" ^^^ Bin./ | "div" ^^^ Bin.div | "mod" ^^^ Bin.mod)

  private def production2 = "+" ~> production3 | rep1("-" ~ "-") ~> production3 | "-" ~> production3 ^^ Unary.- |
                            production3

  private def production3 = rep1sep(production4, "^" | "**") ^^ (_.reduceRight(Bin.^))

  private def production4 = freeTokens | "(" ~> expr <~ ")"

  private def reduction: Parser[Expression] = keyword ~ indexing ~ production1 ^^ {
    case "max" ~ indexing ~ expr  => Reduction.Max(indexing, expr)
    case "min" ~ indexing ~ expr  => Reduction.Min(indexing, expr)
    case "sum" ~ indexing ~ expr  => Reduction.Sum(indexing, expr)
    case "prod" ~ indexing ~ expr => Reduction.Prod(indexing, expr)
  }

  private def freeTokens: Parser[Expression] =
    List(reduction, number, reference) reduce (_ | _)


}

