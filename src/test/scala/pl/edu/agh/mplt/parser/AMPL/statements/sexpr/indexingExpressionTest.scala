package pl.edu.agh.mplt.parser.AMPL.statements.sexpr

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.logical.{Logical, Comparision, LogicalExpressionAMPLParser}
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.{Bin, ArithmeticAMPLParser}
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.set.SetComprehension
import pl.edu.agh.mplt.parser.formula.set.ExplicitSet
import pl.edu.agh.mplt.parser.reference.{ReferenceParser, SimpleReference}

class indexingExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new IndexingAMPLParser with SetExpressionAMPLParser with LogicalExpressionAMPLParser
    with ExpressionAMPLParser with ArithmeticAMPLParser with MemberAMPLParser with ReferenceParser
    with KeywordAMPLParser


  def expr = parser.indexing

  def parse(input: String) = parser.parse(expr, input).get

  "Indexing Parsers" should "parse simple indexing expr" in {
    parse(" { { } }") should be(Indexing(List(ExplicitSet())))
    parse("{ 1 .. 10  }") should be(Indexing(List(SetComprehension(1, 10))))
    parse("{ 1 .. 10 by 3  }") should be(Indexing(List(SetComprehension(1, 10, 3))))

    parse(" { i in { } }") should be(Indexing(List(IndexedSet(List("i"), ExplicitSet()))))
    parse(" { i in 1 .. 10 by 3 }") should be(Indexing(List(IndexedSet(List("i"), SetComprehension(1, 10, 3)))))
  }

  it should "parse indexed sets with setReferences" in {
    parse(" { i in A }") should be(Indexing(List(IndexedSet(List("i"), SimpleReference("A")))))

    parse(" { i in A, j in B }") should be(Indexing(List(
      IndexedSet(List("i"), SimpleReference("A")),
      IndexedSet(List("j"), SimpleReference("B")))))

    parse(" { (i, j) in A }") should be(Indexing(List(IndexedSet(List("i", "j"), SimpleReference("A")))))
  }

  it should "parse indexed sets" in {
    parse("{i in A, (j, k) in B}") should be(
      Indexing(List(
        IndexedSet(List("i"), SimpleReference("A")),
        IndexedSet(List("j", "k"), SimpleReference("B"))
      )))
  }

  it should "parse simple indexing expression with logical condition" in {
    parse("{i in A: i > 5}") should be(
      Indexing(
        List(IndexedSet(List("i"), SimpleReference("A"))),
        Some(Comparision.>("i", 5))))
  }

  it should "parse complex indexing expression with logical condition" in {
    parse("{i in A, (j, k) in B : i == j and i + j > k}") should be(
      Indexing(
        List(
          IndexedSet(List("i"), SimpleReference("A")),
          IndexedSet(List("j", "k"), SimpleReference("B"))),
        Some(Logical.and(
          Comparision.==("i", "j"),
          Comparision.>(Bin.+("i", "j"), "k")))))
  }
}
