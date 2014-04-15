package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.declaration.PiecewiseLinearTerm
import pl.edu.agh.mplt.parser.declaration.objective.Maximize
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.{IndexedSet, Indexing}
import pl.edu.agh.mplt.parser.reference.SimpleReference


class BreakpointsTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = AMPLParser()

  def expr = parser.objectiveDeclaration

  def parse(input: String) = parser.parse(expr, input).get

  "PiecewiseLinearTerm Parser" should "parse simple piecwise linear term" in {
    parse("maximize x : 1 << 1, 2 ; 1, 2 >> 3;") should be(Maximize("x", expression = 1, piecewiseLinearTerms = Some(
      PiecewiseLinearTerm(
        List[(Option[Indexing], Expression)](
          (None, 1),
          (None, 2)),
        List[(Option[Indexing], Expression)](
          (None, 1), (None, 2)),
        (3, None)))))
  }
  it should "parse simple piecwise linear term with indexing" in {
    parse("maximize x : 1 << {i in A} 1, 2; 1, {i in A} 2>> 3, 4;") should be(Maximize("x", expression = 1,
      piecewiseLinearTerms = Some(
        PiecewiseLinearTerm(
          List[(Option[Indexing], Expression)](
            (Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))), 1),
            (None, 2)),
          List[(Option[Indexing], Expression)](
            (None, 1),
            (Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))), 2)),
          (3, Some(4))))))
  }

}
