package pl.edu.agh.mplt.parser.AMPL.statements.lexpr

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.phrase.logical._
import pl.edu.agh.mplt.parser.member.{SetMember, MemberAMPLParser}
import pl.edu.agh.mplt.parser.phrase.set._
import pl.edu.agh.mplt.parser.phrase.expression.{Bin, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.reference.{SimpleReference, ReferenceParser}
import pl.edu.agh.mplt.parser.phrase.set.SetComprehension
import pl.edu.agh.mplt.parser.phrase.set.ExplicitSet

class LogicalExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new ReferenceParser with KeywordAMPLParser with ExpressionAMPLParser with IndexingAMPLParser
    with LogicalExpressionAMPLParser with SetExpressionAMPLParser with MemberAMPLParser

  def expr = parser.lexpr

  def parse(input: String) = parser.parse(expr, input).get

  "Logical Parser" should "parse operator <" in {
    parse("1 < 3") should be(Comparision.<(1, 3))
  }

  it should "parser operator <=" in {
    parse("1 <= 3") should be(Comparision.<=(1, 3))
  }

  it should "parser operator >" in {
    parse("1 > 3") should be(Comparision.>(1, 3))
  }

  it should "parser operator >=" in {
    parse("1 >= 3") should be(Comparision.>=(1, 3))
  }

  it should "parser operator ==" in {
    parse("1 == 3") should be(Comparision.==(1, 3))
  }

  it should "parser operator !=" in {
    parse("1 != 3") should be(Comparision.!=(1, 3))
  }

  it should "parser `<>' same as '!='" in {
    parse("1 <> 3") should be(parse("1 != 3"))
  }

  it should "parser `=' same as '=='" in {
    parse("1 = 3") should be(parse("1 == 3"))
  }

  it should "parse boolean variables " in {
    parse("a > 3") should be(Comparision.>("a", 3))
    parse("b == variable") should be(Comparision.==("b", "variable"))
  }

  it should "parse compund comparrision statements" in {
    parse("1 + 3 >= 7 * z - 5") should be(
      Comparision.>=(
        Bin.+(1, 3),
        Bin.-(Bin.*(7, "z"), 5))
    )
  }

  //////////////////////

  it should "parse number as logical non-zero check" in {
    parse("1") should be(Comparision.!=(1, 0))
  }

  it should "parse not expr" in {
    parse("not x") should be(Logical.Not("x"))
  }

  it should "parse or expr" in {
    parse("x or y") should be(Logical.Or("x", "y"))
  }

  it should "parse and expr" in {
    parse("x and 7") should be(Logical.And("x", Comparision.!=(7, 0)))
  }

  it should "parse '!' as 'not'" in {
    parse("!x") should be(parse("not x"))
  }

  it should "parse '||' as 'or'" in {
    parse("x || y") should be(parse("x or y"))
  }

  it should "parse '&&' as 'and'" in {
    parse("x && 7") should be(parse("x and 7"))
  }


  it should "maintain left associativity of conjunction" in {
    parse("x and y and z") should be(Logical.And(Logical.And("x", "y"), "z"))
  }

  it should "maintain left associativity of alternative" in {
    parse("x or y or z") should be(Logical.Or(Logical.Or("x", "y"), "z"))
  }

  ///////////////////

  it should "parse ands with or" in {
    parse("x and y and z or a") should be(Logical.Or(Logical.And(Logical.And("x", "y"), "z"), "a"))
    parse("x and y or z and a") should be(Logical.Or(Logical.And("x", "y"), Logical.And("z", "a")))
  }

  it should "parse ands with not" in {
    parse("x and y and not z") should be(Logical.And(Logical.And("x", "y"), Logical.Not("z")))
  }

  it should "parse ands with not and or 1" in {
    parse("x and y or not z") should be(Logical.Or(Logical.And("x", "y"), Logical.Not("z")))
    parse("x and not y or z") should be(Logical.Or(Logical.And("x", Logical.Not("y")), "z"))
    parse("not x and  y or z") should be(Logical.Or(Logical.And(Logical.Not("x"), "y"), "z"))
    parse("not x or  y and z") should be(Logical.Or(Logical.Not("x"), Logical.And("y", "z")))
  }

  it should "parse simple member inclusion" in {
    parse("1 in {1, 2, 3}") should be(
      Inclusion.Member(1, ExplicitSet(Set[SetMember](1, 2, 3))))
    parse("1 in 1 .. 3") should be(
      Inclusion.Member(1, SetComprehension(1, 3)))
  }

  it should "parse simple member exclusion" in {
    parse("1 not in {1, 2, 3}") should be {
      Exclusion.Member(1, ExplicitSet(Set[SetMember](1, 2, 3)))
    }
  }

  it should "parse simple subset inclusion" in {
    parse("1 .. 5 within {1, 2, 3}") should be {
      Inclusion.Subset(SetComprehension(1, 5), ExplicitSet(Set[SetMember](1, 2, 3)))
    }
  }

  it should "parse simple subset exclusion" in {
    parse("1 .. 5 not within {1, 2, 3}") should be {
      Exclusion.Subset(SetComprehension(1, 5), ExplicitSet(Set[SetMember](1, 2, 3)))
    }
  }

  ///////////////////

  it should "parse compound logical expression 1" in {
    parse("x and y and not z or a and 16 != 0") should be(
      Logical.Or(
        Logical.And(Logical.And("x", "y"), Logical.Not("z")),
        Logical.And("a", Comparision.!=(16, 0))
      )
    )
  }

  it should "parse compound logical expression 2" in {
    parse("1 in 1 .. 5 and not {1, 2, 3} within 1 .. 4 or {1, 2} not within 1 .. 2 and x > 10 + 5") should be(
      Logical.Or(
        Logical.And(
          Inclusion.Member(1, SetComprehension(1, 5)),
          Logical.Not(
            Inclusion.Subset(
              ExplicitSet(Set[SetMember](1, 2, 3)),
              SetComprehension(1, 4)))),
        Logical.And(
          Exclusion.Subset(
            ExplicitSet(Set[SetMember](1, 2)),
            SetComprehension(1, 2)),
          Comparision.>("x", Bin.+(10, 5))
        )
      ))
  }

  it should "parse forall reduction" in {
    parse("forall {A} 1 == 1") should be(
      LogicalReduction.Forall(
        Indexing(List(SimpleReference("A"))),
        Comparision.==(1, 1)))
  }

  it should "parse exists reduction" in {
    parse("exists {A} 1 == 1") should be(
      LogicalReduction.Exists(
        Indexing(List(SimpleReference("A"))),
        Comparision.==(1, 1)))
  }

  ///////////////////

  "conjunction" should "precede alternative" in {
    parse("x and y or z") should be(Logical.Or(Logical.And("x", "y"), "z"))
  }

  "negation" should "precede conjunction" in {
    parse("x and not y") should be(Logical.And("x", Logical.Not("y")))
  }


}
