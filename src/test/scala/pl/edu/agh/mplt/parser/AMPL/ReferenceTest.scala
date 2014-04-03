package pl.edu.agh.mplt.parser.AMPL

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.reference.{IndexedReference, SimpleReference, ReferenceParser}
import pl.edu.agh.mplt.parser.formula.expression.{Expression, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.formula.set.{SetExpressionAMPLParser, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser


class ReferenceTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new ReferenceParser with KeywordAMPLParser with ExpressionAMPLParser with IndexingAMPLParser
    with LogicalExpressionAMPLParser with SetExpressionAMPLParser with MemberAMPLParser

  def expr = parser.reference

  def parse(input: String) = parser.parse(expr, input).get

  "Reference parser" should "parse simple reference" in {
    parse("a") should be(SimpleReference("a"))
    parse("costam") should be(SimpleReference("costam"))
  }

  it should "parse indexed reference" in {
    parse("a[3]") should be(IndexedReference("a", List[Expression](3)))
    parse("costam[a]") should be(IndexedReference("costam", List[Expression]("a")))
  }

  it should "parse indexed reference with multiple indexes" in {
    parse("a[3,4]") should be(IndexedReference("a", List[Expression](3, 4)))
    parse("costam[a, b]") should be(IndexedReference("costam", List[Expression]("a", "b")))
  }
}
