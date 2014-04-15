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

  def parse(input: String) = parser.parse(expr, input)

  "PiecewiseLinearTerm Parser" should "parse simple piecwise linear term" in {
    parse("maximize x : 1 << 1, 2 ; 1, 2 >> 3;").get should be(Maximize("x", expression = Some(1), piecewiseLinearTerms = Some(
      PiecewiseLinearTerm(
        List[(Option[Indexing], Expression)](
          (None, 1),
          (None, 2)),
        List[(Option[Indexing], Expression)](
          (None, 1), (None, 2)),
        (3, None)))))
  }
  it should "parse simple piecwise linear term with indexing" in {
    parse("maximize x : 1 << {i in A} 1, 2; 1, {i in A} 2>> 3, 4;").get should be(Maximize("x", expression = Some(1),
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

  it should "parse complex objective with breakpoint" in {
    parse(
      """
        |maximize Total_Profit:
        |   sum {p in PROD, t in 1..T} (revenue[p,t]*Sell[p,t] -
        |      prodcost[p]*Make[p,t] - invcost[p]*Inv[p,t])
        | - sum {t in 1..T} <<avail_min[t]; 0,time_penalty[t]>> Use[t]
        | ;
      """.stripMargin).get should be (1)

//    match {
//      case parser.Success(_, _) =>
//      case _ => throw new Exception
//    }
  }
}
