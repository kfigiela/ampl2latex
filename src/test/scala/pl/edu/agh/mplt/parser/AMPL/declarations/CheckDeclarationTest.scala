package pl.edu.agh.mplt.parser.AMPL.declarations

import pl.edu.agh.mplt.parser.{IntercodeImplicits, AMPLParser}
import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.declaration.assertion.Assertion
import pl.edu.agh.mplt.parser.formula.logical.Comparision
import pl.edu.agh.mplt.parser.formula.set.{IndexedSet, Indexing}
import pl.edu.agh.mplt.parser.reference.SimpleReference


class CheckDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = AMPLParser()

  def expr = parser.check

  def parse(input: String) = parser.parse(expr, input).get

  "Check Parser" should "parse simple check declaration" in {
    parse("check 1 == 1;") should be(Assertion(None, Comparision.==(1, 1)))
  }

  it should "parse check declaration with colon" in {
    parse("check : 1 == 1;") should be(Assertion(None, Comparision.==(1, 1)))
  }

  it should "parse check declaration with indexing" in {
    parse("check {i in A} : i == 1;") should be(Assertion(
      Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
      Comparision.==("i", 1)))
  }
}
