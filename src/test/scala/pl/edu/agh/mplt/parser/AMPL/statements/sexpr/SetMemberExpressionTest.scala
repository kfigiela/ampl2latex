package pl.edu.agh.mplt.parser.AMPL.statements.sexpr

import pl.edu.agh.mplt.parser.member._
import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.phrase.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.reference.ReferenceParser
import pl.edu.agh.mplt.parser.member.StringMember
import pl.edu.agh.mplt.parser.member.ExpressionMember
import pl.edu.agh.mplt.parser.member.MultiMember
import pl.edu.agh.mplt.parser.phrase.set.{SetExpressionAMPLParser, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.phrase.logical.LogicalExpressionAMPLParser


class SetMemberExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {

  val parser = new ReferenceParser with KeywordAMPLParser with ExpressionAMPLParser with IndexingAMPLParser
    with LogicalExpressionAMPLParser with SetExpressionAMPLParser with MemberAMPLParser

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
    parse( """( 1, 2)""") should be(MultiMember(List[SetMember](1, 2)))
  }

  it should "parse mixed multidimensional member" in {
    parse( """( 1, "a" )""") should be(MultiMember(List[SetMember](1, StringMember("a"))))
  }

  it should "parse set reference" in {
    parse("a") should be(ExpressionMember("a"))
  }

}
