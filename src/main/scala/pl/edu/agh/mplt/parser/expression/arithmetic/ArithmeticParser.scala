package pl.edu.agh.mplt.parser.expression.arithmetic

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.expression.Expression

trait ArithmeticParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def arithmeticExpression: Parser[Expression] = binExpr

  private def binExpr = chainl1(p1, "+" ^^^ Bin.+ | "-" ^^^ Bin.- | "less" ^^^ Bin.less)

  private def p1 = chainl1(p2, "*" ^^^ Bin.* | "/" ^^^ Bin./ | "div" ^^^ Bin.div | "mod" ^^^ Bin.mod)

  def p2 = "+" ~> p3 | rep1("-" ~ "-") ~> p3 | "-" ~> p3 ^^ Unary.- | p3

  private def p3 = rep1sep(p4, "^" | "**") ^^ (_.reduceRight(Bin.^))

  private def p4 = number | "(" ~> expr <~ ")" | expr

  def number = floatingPointNumber ^^ Number

}
