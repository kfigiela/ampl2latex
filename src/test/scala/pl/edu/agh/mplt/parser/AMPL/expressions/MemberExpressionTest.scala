package pl.edu.agh.mplt.parser.AMPL.expressions

import pl.edu.agh.mplt.parser.member.{MemberAMPLParser, StringMember}
import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.ArithmeticAMPLParser


class MemberExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {

  val parser = new MemberAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser

  def expr = parser.member

  def parse(input: String) = parser.parse(expr, input).get

  it should "string Member" in {
    parse( """ "A" """) should be(StringMember("A"))
    parse( """ "AlaMaKota" """) should be(StringMember("AlaMaKota"))
    parse( """ "AlaMaKota" """) should not be StringMember("Ala Ma Kota")
    parse("A") should not be StringMember("A")
  }
}
