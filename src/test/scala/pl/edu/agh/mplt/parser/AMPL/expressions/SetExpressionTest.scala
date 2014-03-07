package pl.edu.agh.mplt.parser.AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.expression.set.{SetExpressionAMPLParser, ExplicitSet, SetComprehension}
import pl.edu.agh.mplt.parser.expression.{StringLiteral, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.expression.arithmetic.ArithmeticAMPLParser
import pl.edu.agh.mplt.parser.expression.Number

class SetExpressionTest extends FlatSpec with Matchers {
  implicit def intToString(n: Int): String = n.toString

  implicit def intToNumber(n: Int): Number = Number(n)

  implicit def stringToStringLiteral(str: String): StringLiteral = StringLiteral(str)

  val parser = new SetExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser

  def expr = parser.sexpr

  def parse(input: String) = parser.parse(expr, input).get

  "Set expression parser" should "parse explicit number set definition" in {
        parse("{1, 2, 3}") should be(ExplicitSet(Set[Number](1, 2, 3)))
  }

  it should "parse one element set literal" in {
    parse("{ \"a\" }") should be(ExplicitSet(Set[StringLiteral]("a")))
  }

  it should "parse explicit string literal set definition" in {
    parse("{\"a\", \"b\", \"c\"}") should be(ExplicitSet(Set[StringLiteral]("a", "b", "c")))
  }

  it should "parse empty set literal" in {
    parse("{}") should be(ExplicitSet(Set.empty))
  }

  it should "parse number set comprehension" in {
    parse("1 .. 10") should be(SetComprehension(1, 10))
  }

  it should "parse string set comprehension" in {
        parse( """ "a" .. "f" """) should be(SetComprehension("a", "f"))
  }

  it should "parse number set comprehension with step" in {
    parse("1 .. 17 by 5") should be(SetComprehension(1, 17, 5))
  }

  it should "parse string set comprehension with step" in {
        parse( """ "a" .. "d" by 5""") should be(SetComprehension("a", "d", 5))
  }

}
