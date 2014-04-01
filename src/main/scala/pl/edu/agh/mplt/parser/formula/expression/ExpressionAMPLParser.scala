package pl.edu.agh.mplt.parser.formula.expression

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.reference.Reference

trait ExpressionAMPLParser extends JavaTokenParsers {

  def reference: Parser[Reference]

  def expr: Parser[Expression] = arithmeticExpression | freeTokens

  def number: Parser[Number] = floatingPointNumber ^^ Number

  private def arithmeticExpression: Parser[Expression] =
    chainl1(p1, "+" ^^^ Bin.+ | "-" ^^^ Bin.- | "less" ^^^ Bin.less)

  private def p1 = chainl1(p2, "*" ^^^ Bin.* | "/" ^^^ Bin./ | "div" ^^^ Bin.div | "mod" ^^^ Bin.mod)

  private def p2 = "+" ~> p3 | rep1("-" ~ "-") ~> p3 | "-" ~> p3 ^^ Unary.- | p3

  private def p3 = rep1sep(p4, "^" | "**") ^^ (_.reduceRight(Bin.^))

  private def p4 = freeTokens | "(" ~> expr <~ ")"

  private def freeTokens: Parser[Expression] =
    List(number, reference) reduce (_ | _)


}

