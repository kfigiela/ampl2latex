package pl.edu.agh.mplt.parser.AMPL

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.declaration.set.{SetDeclaration, SetDeclarationParser}
import pl.edu.agh.mplt.parser.declaration.set.attributes.AttributesAMPLParser
import pl.edu.agh.mplt.parser.set.indexing.{Indexing, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.set.{ExplicitSet, SetComprehension, SetExpressionAMPLParser}
import pl.edu.agh.mplt.parser.expression.{StringLiteral, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.expression.arithmetic.{Bin, ArithmeticAMPLParser}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.expression.Number


class SetDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {

  val parser = new SetDeclarationParser with IndexingAMPLParser with SetExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser with LogicalExpressionAMPLParser with AttributesAMPLParser

  def expr = parser.declaration

  def parse(input: String) = parser.parse(expr, input).get

  "Set declaration parser" should "parse simple set declaration" in {
    parse("set nodes;") should be(SetDeclaration("nodes"))
  }

  it should "parse set declaration with alias" in {
    parse("set oranges apples;") should be(SetDeclaration("oranges", Some("apples")))
  }

  it should "parse set declaration with simple indexing" in {
    parse("set apples { 1 + 3 .. 10 by 4 };") should be(SetDeclaration("apples", indexing = Some(Indexing(SetComprehension(Bin.+(1, 3), 10, 4)))))
  }

  it should "parse set declaration with indexing" in {
    parse( """set apples { 1 + 3 .. 10 by 4 , {1, 2, 3}, {"a", "b", "c"} };""") should be(
      SetDeclaration("apples", indexing = Some(Indexing(List(
        SetComprehension(Bin.+(1, 3), 10, 4),
        ExplicitSet(Set[Number](1, 2, 3)),
        ExplicitSet(Set[StringLiteral]("a", "b", "c")))))))
  }

  it should "parse set declaration with simple attribute" in {
    //    parse("set arcs within nodes cross nodes;") should be
    //    (SetDeclarationTest.super("arcs", attributes = List(SetAttribute.is(SetExpression.cross(Variable("nodes"), Variable("nodes"))))))
  }

}
