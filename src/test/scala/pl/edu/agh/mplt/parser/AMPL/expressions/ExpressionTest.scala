package pl.edu.agh.mplt.parser.AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.formula.expression.{Number, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.{ArithmeticAMPLParser, Unary}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.reference.{ReferenceParser, NumberReference}

class ExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new ExpressionAMPLParser with ArithmeticAMPLParser with ReferenceParser

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
    parse("A") should be(NumberReference("A"))
    parse("AlaMaKota") should be(NumberReference("AlaMaKota"))
    parse("Ala Ma Kota") should not be NumberReference("Ala Ma Kota")
    parse("12") should not be NumberReference("12")
  }


}
