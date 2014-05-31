package pl.edu.agh.mplt.parser.AMPL.statements.expr

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.reference.{IndexedReference, SimpleReference, ReferenceParser}
import pl.edu.agh.mplt.parser.phrase.expression.Number
import pl.edu.agh.mplt.parser.phrase.set.{SetExpressionAMPLParser, IndexingAMPLParser, IndexedSet, Indexing}
import pl.edu.agh.mplt.parser.phrase.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.member.MemberAMPLParser

class ArithmeticExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new ReferenceParser with KeywordAMPLParser with ExpressionAMPLParser with IndexingAMPLParser
    with LogicalExpressionAMPLParser with SetExpressionAMPLParser with MemberAMPLParser

  def expr = parser.expr

  def parse(input: String) = parser.parse(expr, input).get

  "Arithmetic parser" should "parse addition" in {
    parse("1 + 2") should be(Bin.+(1, 2))
  }

  it should "parse subtraction" in {
    parse("1 - 2") should be(Bin.-(1, 2))
  }

  it should "parse less operator" in {
    parse("1 less 2") should be(Bin.Less(1, 2))
  }

  it should "parser reduction" in {
    parse("sum {i in A} i*i") should be(ExpressionReduction.Sum(
      Indexing(List(IndexedSet(List("i"), SimpleReference("A")))),
      Bin.*(SimpleReference("i"), SimpleReference("i"))
    ))
  }

  it should "parse multiplication" in {
    parse("1 * 2") should be(Bin.*(1, 2))
  }

  it should "parse division" in {
    parse("1 / 2") should be(Bin./(1, 2))
  }

  it should "parse modulo operator" in {
    parse("1 mod 2") should be(Bin.Mod(1, 2))
  }

  it should "parse integer division" in {
    parse("1 div 2") should be(Bin.Div(1, 2))
  }

  it should "parse power operator" in {
    parse("1 ^ 2") should be(Bin.^(1, 2))
  }

  it should """parse "**" operator same as"^" """ in {
    parse("1 ** 2") should be(parse("1 ^ 2"))
  }

  it should "parse unary +" in {
    parse("+1") should be(Number(1))
  }

  it should "parse unary -" in {
    parse("-1") should be(Unary.-(Number(1)))
  }

  it should "parse parenthesis" in {
    parse("(1 + 2) * 3") should be(Bin.*(ParenthesizedExpression(Bin.+(1, 2)), 3))
  }

  ////////////////////////////////////////////

  it should "maintain left associativity of addition" in {
    parse("1 + 2 + 3") should be(Bin.+(Bin.+(1, 2), 3))
  }

  it should "maintain left associativity of subtraction" in {
    parse("1 - 2 - 3") should be(Bin.-(Bin.-(1, 2), 3))
  }

  it should "maintain left associativity of less operator" in {
    parse("1 less 2 less 3") should be(Bin.Less(Bin.Less(1, 2), 3))
  }

  it should "maintain left associativity of multiplication" in {
    parse("1 * 2 * 3") should be(Bin.*(Bin.*(1, 2), 3))
  }

  it should "maintain left associativity of division" in {
    parse("1 / 2 / 3") should be(Bin./(Bin./(1, 2), 3))
  }

  it should "maintain left associativity of modulo" in {
    parse("1 mod 2 mod 3") should be(Bin.Mod(Bin.Mod(1, 2), 3))
  }

  it should "maintain left associativity of integer division" in {
    parse("1 div 2 div 3") should be(Bin.Div(Bin.Div(1, 2), 3))
  }

  it should "maintain right associativity of exponentiation" in {
    parse("1 ^ 2 ^ 3") should be(Bin.^(1, Bin.^(2, 3)))
  }

  it should "maintain left associativity of unary addition" in {
    parse("+1 - 2") should be(Bin.-(1, 2))
  }

  it should "maintain left associativity of unary subtraction" in {
    parse("-1 + 2") should be(Bin.+(Unary.-(1), 2))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "multiplication" should "precede addition" in {
    parse("1 + 2 * 3") should be(Bin.+(1, Bin.*(2, 3)))
  }

  it should "precede subtraction" in {
    parse("1 - 2 * 3") should be(Bin.-(1, Bin.*(2, 3)))
  }

  it should "precede less" in {
    parse("1 less 2 * 3") should be(Bin.Less(1, Bin.*(2, 3)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "division" should "precede addition" in {
    parse("5 + 3 / 2") should be(Bin.+(5, Bin./(3, 2)))
  }

  it should "precede subtraction" in {
    parse("5 - 3 / 2") should be(Bin.-(5, Bin./(3, 2)))
  }

  it should "precede less" in {
    parse("5 less 3 / 2") should be(Bin.Less(5, Bin./(3, 2)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "integer division" should "precede addition" in {
    parse("5 + 3 div 2") should be(Bin.+(5, Bin.Div(3, 2)))
  }

  it should "precede subtraction" in {
    parse("5 - 3 div 2") should be(Bin.-(5, Bin.Div(3, 2)))
  }

  it should "precede less" in {
    parse("5 less 3 div 2") should be(Bin.Less(5, Bin.Div(3, 2)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "modulo" should "precede addition" in {
    parse("5 + 3 mod 2") should be(Bin.+(5, Bin.Mod(3, 2)))
  }

  it should "precede subtraction" in {
    parse("5 - 3 mod 2") should be(Bin.-(5, Bin.Mod(3, 2)))
  }

  it should "precede less" in {
    parse("5 less 3 mod 2") should be(Bin.Less(5, Bin.Mod(3, 2)))
  }
  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "unary minus" should "precede division" in {
    parse("-1 / 3") should be(Bin./(Unary.-(1), 3))
  }

  it should "precede integer division" in {
    parse("-1 div 3") should be(Bin.Div(Unary.-(1), 3))
  }

  it should "precede modulo" in {
    parse("-1 mod 3") should be(Bin.Mod(Unary.-(1), 3))
  }

  it should "precede multiplication" in {
    parse("-1 * 3") should be(Bin.*(Unary.-(1), 3))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "exponentiation" should "precede unary plus" in {
    parse("+1 ^ 3") should be(Bin.^(1, 3))
  }

  it should "precede unary minus" in {
    parse("-1 ^ 3") should be(Unary.-(Bin.^(1, 3)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  it should "parse complex reduction" in {
    parse("sum {i in A} cost[i]*make[i]") should be(
      ExpressionReduction.Sum(
        Indexing(List(IndexedSet(List("i"), SimpleReference("A")))),
        Bin.*(
          IndexedReference(SimpleReference("cost"), List(SimpleReference("i"))),
          IndexedReference(SimpleReference("make"), List(SimpleReference("i"))))))
  }

  it should "parse more complex sum" in {
    parser.parse(expr, """
                         |   sum {p in PROD, t in 1..T} (revenue[p,t]*Sell[p,t] -
                         |      prodcost[p]*Make[p,t] - invcost[p]*Inv[p,t])
                         | - sum {t in 1..T}
                       """.stripMargin) match {
      case parser.Success(_, _) =>
      case _ => throw new Exception
    }
  }

}
