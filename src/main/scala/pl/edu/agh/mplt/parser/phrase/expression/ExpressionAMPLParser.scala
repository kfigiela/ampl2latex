package pl.edu.agh.mplt.parser.phrase.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.parser.phrase.set.Indexing
import pl.edu.agh.mplt.parser.phrase.logical.LogicalExpression
import language.postfixOps

trait ExpressionAMPLParser extends JavaTokenParsers {

  def reference: Parser[Reference]

  def indexing: Parser[Indexing]

  def expr: Parser[Expression] = arithmeticExpression | freeTokens

  def number: Parser[Number] = """-?(\d+(\.\d+)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ Number

  def functionName: Parser[String]

  def exprReductionOp: Parser[String]

  def lexpr: Parser[LogicalExpression]

  private def ifExpr: Parser[Expression] = "if" ~> lexpr ~ ("then" ~> expr) ~ (("else" ~> expr) ?) ^^ {
    case lexpr ~  t ~ None => ExpressionIf(lexpr, t)
    case lexpr ~  t ~ Some(f) => ExpressionIf(lexpr, t, f)
  }

  private def arithmeticExpression: Parser[Expression] =
    chainl1(production1, "+" ^^^ Bin.+ | "-" ^^^ Bin.- | "less" ^^^ Bin.Less)

  private def production1 = chainl1(production2, "*" ^^^ Bin.* | "/" ^^^ Bin./ | "div" ^^^ Bin.Div | "mod" ^^^ Bin.Mod)

  private def production2 = "+" ~> production3 | rep1("-" ~ "-") ~> production3 | "-" ~> production3 ^^ Unary.- |
    production3

  private def production3 = rep1sep(production4, "^" | "**") ^^ (_.reduceRight(Bin.^))

  private def production4 = freeTokens | "(" ~> expr <~ ")" ^^ ParenthesizedExpression

  private def reduction: Parser[Expression] = exprReductionOp ~ indexing ~ production1 ^? {
    case "max" ~ indexing ~ expr => ExpressionReduction.Max(indexing, expr)
    case "min" ~ indexing ~ expr => ExpressionReduction.Min(indexing, expr)
    case "sum" ~ indexing ~ expr => ExpressionReduction.Sum(indexing, expr)
    case "prod" ~ indexing ~ expr => ExpressionReduction.Prod(indexing, expr)
  }

  private def function: Parser[Expression] = functionName ~ "(" ~ repsep(expr, ",") <~ ")" ^^ {
    case name ~ "(" ~ args => FunctionCall(name, args)
  }

  private def freeTokens: Parser[Expression] =
    List(ifExpr, reduction, function, number, reference) reduce (_ | _)


}
