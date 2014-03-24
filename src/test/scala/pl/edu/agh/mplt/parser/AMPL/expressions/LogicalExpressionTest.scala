package pl.edu.agh.mplt.parser.AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.formula.logical._
import pl.edu.agh.mplt.parser.member.{Member, MemberAMPLParser}
import pl.edu.agh.mplt.parser.formula.set.SetExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.{Bin, ArithmeticAMPLParser}
import pl.edu.agh.mplt.parser.reference.ReferenceParser
import pl.edu.agh.mplt.parser.formula.set.SetComprehension
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.formula.set.ExplicitSet

class LogicalExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new LogicalExpressionAMPLParser with SetExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser with MemberAMPLParser with ReferenceParser

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

  it should "parse compund comparrision expressions" in {
    parse("(1 + 3) >= (7 * a) - 5") should be(
      Comparision.>=(
        Bin.+(1, 3),
        Bin.-(Bin.*(7, "a"), 5)))
  }

  //////////////////////

  it should "parse number as logical non-zero check" in {
    parse("1") should be(Comparision.!=(1, 0))
  }

  it should "parse not expression" in {
    parse("not x") should be(Logical.not("x"))
  }

  it should "parse or expression" in {
    parse("x or y") should be(Logical.or("x", "y"))
  }

  it should "parse and expression" in {
    parse("x and 7") should be(Logical.and("x", Comparision.!=(7, 0)))
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

  it should "parse compound comparrisions" in {
    parse("x and y and not z or a and 16") should be(
      Logical.or(
        Logical.and("x", Logical.and("y", Logical.not("z"))),
        Logical.and("a", Comparision.!=(16, 0))))
  }

  it should "maintain left associativity of conjunction" in {
    parse("x and y and z") should be(Logical.and("x", Logical.and("y", "z")))
  }

  it should "maintain left associativity of alternative" in {
    parse("x or y or z") should be(Logical.or("x", Logical.or("y", "z")))
  }

  ///////////////////

  it should "parse simple member inclusion" in {
    parse("1 in {1, 2, 3}") should be(
      Inclusion.member(1, ExplicitSet(Set[Member](1, 2, 3))))
    parse("1 in 1 .. 3") should be(
      Inclusion.member(1, SetComprehension(1, 3)))
  }

  it should "parse simple member exclusion" in {
    parse("1 not in {1, 2, 3}") should be {
      Exclusion.member(1, ExplicitSet(Set[Member](1, 2, 3)))
    }
  }

  it should "parse simple subset inclusion" in {
    parse("{1 .. 5} within {1, 2, 3}") should be {
      Inclusion.subset(SetComprehension(1, 5), ExplicitSet(Set[Member](1, 2, 3)))
    }
  }

  it should "parse simple subset exclusion" in {
    parse("{1 .. 5} not within {1, 2, 3}") should be {
      Exclusion.subset(SetComprehension(1, 5), ExplicitSet(Set[Member](1, 2, 3)))
    }
  }

  ///////////////////

  it should "parse compund logical expression" in {
    parse("1 in 1 .. 5 and not {1, 2, 3} within 1 .. 4 or {1, 2} not within 1 .. 2 and x > 10 + 5") should be(
      Logical.or(
        Logical.and(
          Inclusion.member(1, SetComprehension(1, 5)),
          Logical.not(
            Inclusion.subset(
              ExplicitSet(Set[Member](1, 2, 3)),
              SetComprehension(1, 4)))),
        Logical.and(
          Exclusion.subset(
            ExplicitSet(Set[Member](1, 2)),
            SetComprehension(1, 2)),
          Comparision.>("x", Bin.+(10, 5))
        )
      ))
  }

  ///////////////////

  "conjunction" should "precede alternative" in {
    parse("x and y or z") should be(Logical.or(Logical.and("x", "y"), "z"))
  }

  "negation" should "precede conjunction" in {
    parse("x and not y") should be(Logical.and("x", Logical.not("y")))
  }
}
