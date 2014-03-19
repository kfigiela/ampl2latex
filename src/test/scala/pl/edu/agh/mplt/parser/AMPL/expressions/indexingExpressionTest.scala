package pl.edu.agh.mplt.parser.AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.set.indexing.IndexingAMPLParser
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.ArithmeticAMPLParser
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.set.SetComprehension
import pl.edu.agh.mplt.parser.formula.set.ExplicitSet
import pl.edu.agh.mplt.parser.reference.SetReference

class indexingExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new IndexingAMPLParser with SetExpressionAMPLParser with LogicalExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser with MemberAMPLParser

  def expr = parser.indexing

  def parse(input: String) = parser.parse(expr, input).get

  "Indexing Parsers" should "parse simple indexing expression" in {
    parse(" { { } }") should be(Indexing(List(ExplicitSet())))
    parse("{ 1 .. 10  }") should be(Indexing(List(SetComprehension(1, 10))))
    parse("{ 1 .. 10 by 3  }") should be(Indexing(List(SetComprehension(1, 10, 3))))

    parse(" { i in { } }") should be(Indexing(List(IndexedSet(List("i"), ExplicitSet()))))
    parse(" { i in 1 .. 10 by 3 }") should be(Indexing(List(IndexedSet(List("i"), SetComprehension(1, 10, 3)))))
  }

  it should "parse indexed sets with setReferences" in {
    parse(" { i in A }") should be(Indexing(List(IndexedSet(List("i"), SetReference("A")))))
    parse(" { i in A, j in B }") should be(Indexing(List(
      IndexedSet(List("i"), SetReference("A")),
      IndexedSet(List("j"), SetReference("B")))))
    parse(" { (i, j) in A }") should be(Indexing(List(IndexedSet(List("i", "j"), SetReference("A")))))
  }

  it should "parse indexed sets" in {
    parse("{i in A, (i, j) in B}") should be(
      Indexing(List(
        IndexedSet(List("i"), SetReference("A")),
        IndexedSet(List("i", "j"), SetReference("B"))
      )))
  }
}
