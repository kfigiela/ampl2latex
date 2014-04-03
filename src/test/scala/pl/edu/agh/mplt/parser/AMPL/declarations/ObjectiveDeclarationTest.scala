package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.formula.set.{IndexedSet, Indexing, SetExpressionAMPLParser, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.reference.{SimpleReference, ReferenceParser}
import pl.edu.agh.mplt.parser.declaration.objective.{Maximize, Minimize, ObjectiveDeclarationAMPLParser}


class ObjectiveDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser =AMPLParser()

  def expr = parser.objectiveDeclaration

  def parse(input: String) = parser.parse(expr, input).get


  "objective parser" should "parser minimize objective" in {
    parse("minimize x : 1 ;") should be(Minimize("x", expression = 1))
  }

  it should "parser maximize objective" in {
    parse("maximize x : 1 ;") should be(Maximize("x", expression = 1))
  }

  it should "parser maximize objective with an alias" in {
    parse("maximize x y: 1 ;") should be(Maximize("x", Some("y"), expression = 1))
  }

  it should "parser maximize objective with indexing" in {
    parse("maximize x {i in A} : i ;") should be(
      Maximize("x",
        indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A"))))),
        expression = "i"))
  }
}