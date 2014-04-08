package pl.edu.agh.mplt.parser.AMPL.statements.expr

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.formula.expression._
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.reference.{IndexedReference, SimpleReference, ReferenceParser}
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.formula.set.{SetExpressionAMPLParser, IndexingAMPLParser, IndexedSet, Indexing}
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
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
    parse("1 less 2") should be(Bin.less(1, 2))
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
    parse("1 mod 2") should be(Bin.mod(1, 2))
  }

  it should "parse integer division" in {
    parse("1 div 2") should be(Bin.div(1, 2))
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
    parse("(1 + 2) * 3") should be(Bin.*(Bin.+(1, 2), 3))
  }

  ////////////////////////////////////////////

  it should "maintain left associativity of addition" in {
    parse("1 + 2 + 3") should be(Bin.+(Bin.+(1, 2), 3))
  }

  it should "maintain left associativity of subtraction" in {
    parse("1 - 2 - 3") should be(Bin.-(Bin.-(1, 2), 3))
  }

  it should "maintain left associativity of less operator" in {
    parse("1 less 2 less 3") should be(Bin.less(Bin.less(1, 2), 3))
  }

  it should "maintain left associativity of multiplication" in {
    parse("1 * 2 * 3") should be(Bin.*(Bin.*(1, 2), 3))
  }

  it should "maintain left associativity of division" in {
    parse("1 / 2 / 3") should be(Bin./(Bin./(1, 2), 3))
  }

  it should "maintain left associativity of modulo" in {
    parse("1 mod 2 mod 3") should be(Bin.mod(Bin.mod(1, 2), 3))
  }

  it should "maintain left associativity of integer division" in {
    parse("1 div 2 div 3") should be(Bin.div(Bin.div(1, 2), 3))
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
    parse("1 less 2 * 3") should be(Bin.less(1, Bin.*(2, 3)))
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
    parse("5 less 3 / 2") should be(Bin.less(5, Bin./(3, 2)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "integer division" should "precede addition" in {
    parse("5 + 3 div 2") should be(Bin.+(5, Bin.div(3, 2)))
  }

  it should "precede subtraction" in {
    parse("5 - 3 div 2") should be(Bin.-(5, Bin.div(3, 2)))
  }

  it should "precede less" in {
    parse("5 less 3 div 2") should be(Bin.less(5, Bin.div(3, 2)))
  }

  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "modulo" should "precede addition" in {
    parse("5 + 3 mod 2") should be(Bin.+(5, Bin.mod(3, 2)))
  }

  it should "precede subtraction" in {
    parse("5 - 3 mod 2") should be(Bin.-(5, Bin.mod(3, 2)))
  }

  it should "precede less" in {
    parse("5 less 3 mod 2") should be(Bin.less(5, Bin.mod(3, 2)))
  }
  ////////////////////////////////////////////
  ////////////////////////////////////////////

  "unary minus" should "precede division" in {
    parse("-1 / 3") should be(Bin./(Unary.-(1), 3))
  }

  it should "precede integer division" in {
    parse("-1 div 3") should be(Bin.div(Unary.-(1), 3))
  }

  it should "precede modulo" in {
    parse("-1 mod 3") should be(Bin.mod(Unary.-(1), 3))
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

}
