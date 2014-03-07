package AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.expression.{StringLiteral, Number, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.expression.arithmetic.{ArithmeticAMPLParser, Unary}
import pl.edu.agh.mplt.parser.expression.variable.Variable


class ExpressionTest extends FlatSpec with Matchers {
  implicit def intToString(i: Int): String = i.toString

  implicit def intToNumber(i: Int): Number = Number(i)

  implicit def doubleToNumber(i: Double): Number = Number(i.toString)

  val parser = new ExpressionAMPLParser with ArithmeticAMPLParser

  def expr = parser.expr

  def parse(input: String) = parser.parse(expr, input).get

  "Expression Parser" should "parse numbers" in {
    parse("1") should be(Number(1))
    parse("1.1") should be(Number("1.1"))
    parse("0.0") should be(Number("0.0"))
    parse("-3.16") should be(Unary.-(Number("3.16")))
    parse("-3.16e10") should be(Unary.-(Number("3.16e10")))
  }

  it should "parse variable" in {
    parse("A") should be(Variable("A"))
    parse("AlaMaKota") should be(Variable("AlaMaKota"))
    parse("Ala Ma Kota") should not be Variable("Ala Ma Kota")
    parse( """ "A" """) should not be Variable("A")
    parse("12") should not be Variable("12")
  }

  it should "string literals" in {
    parse( """ "A" """) should be(StringLiteral("A"))
    parse( """ "AlaMaKota" """) should be(StringLiteral("AlaMaKota"))
    parse( """ "AlaMaKota" """) should not be StringLiteral("Ala Ma Kota")
    parse("A") should not be StringLiteral("A")
  }
}
