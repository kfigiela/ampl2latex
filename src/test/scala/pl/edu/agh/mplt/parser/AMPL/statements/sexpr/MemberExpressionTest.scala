package pl.edu.agh.mplt.parser.AMPL.statements.sexpr

import pl.edu.agh.mplt.parser.member._
import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.reference.ReferenceParser
import pl.edu.agh.mplt.parser.member.StringMember
import pl.edu.agh.mplt.parser.member.ExpressionMember
import pl.edu.agh.mplt.parser.member.MultiMember


class MemberExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {

  val parser = new MemberAMPLParser with ExpressionAMPLParser
    with ReferenceParser with KeywordAMPLParser

  def expr = parser.member

  def parse(input: String) = parser.parse(expr, input).get


  "Member parser" should "parse string Member" in {
    parse( """ "A" """) should be(StringMember("A"))
    parse( """ "AlaMaKota" """) should be(StringMember("AlaMaKota"))
    parse( """ "AlaMaKota" """) should not be StringMember("Ala Ma Kota")
    parse("A") should not be StringMember("A")
  }

  it should "parse expression as a member" in {
    parse("1") should be(ExpressionMember(1))
  }

  it should "parse multidimensional member" in {
    parse( """( 1, 2)""") should be(MultiMember(Seq[Member](1, 2)))
  }

  it should "parse mixed multidimensional member" in {
    parse( """( 1, "a" )""") should be(MultiMember(Seq[Member](1, StringMember("a"))))
  }

}
