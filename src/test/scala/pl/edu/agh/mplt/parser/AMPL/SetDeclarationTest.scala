package pl.edu.agh.mplt.parser.AMPL

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.declaration.set.{SetDeclaration, SetDeclarationParser}
import pl.edu.agh.mplt.parser.declaration.set.attributes.AttributesAMPLParser
import pl.edu.agh.mplt.parser.formula.set.indexing.IndexingAMPLParser
import pl.edu.agh.mplt.parser.formula.set.{ExplicitSet, SetComprehension, SetExpressionAMPLParser}
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.{Bin, ArithmeticAMPLParser}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.member.{StringMember, MemberAMPLParser, Member}
import pl.edu.agh.mplt.parser.formula.set.Indexing


class SetDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {

  val parser = new SetDeclarationParser with IndexingAMPLParser with SetExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser with LogicalExpressionAMPLParser with AttributesAMPLParser with MemberAMPLParser

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
        ExplicitSet(Set[Member](Number(1), Number(2), Number(3))),
        ExplicitSet(Set[Member](StringMember("a"), StringMember("b"), StringMember("c"))))))))
  }

  it should "parse set declaration with simple attribute" in {
    //    parse("set arcs within nodes cross nodes;") should be
    //    (SetDeclarationTest.super("arcs", attributes = List(SetAttribute.is(SetExpression.cross(Variable("nodes"), Variable("nodes"))))))
  }

}
