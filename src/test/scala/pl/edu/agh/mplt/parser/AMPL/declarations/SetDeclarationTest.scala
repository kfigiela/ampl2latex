package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.expression.{Bin, Number}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.member.Member
import pl.edu.agh.mplt.parser.reference.SimpleReference
import pl.edu.agh.mplt.parser.formula.set.SetComprehension
import pl.edu.agh.mplt.parser.formula.set.Indexing
import scala.Some
import pl.edu.agh.mplt.parser.member.StringMember
import pl.edu.agh.mplt.parser.formula.set.ExplicitSet
import pl.edu.agh.mplt.parser.declaration.datatype.{SetDeclaration, Attribute}


class SetDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {

  val parser = AMPLParser()

  def expr = parser.datatypeDeclaration

  def parse(input: String) = parser.parse(expr, input).get

  "Set declaration parser" should "parse simple set declaration" in {
    parse("set nodes;") should be(SetDeclaration("nodes"))
  }

  it should "parse set declaration with alias" in {
    parse("set oranges apples;") should be(SetDeclaration("oranges", Some("apples")))
  }

  ////////////////////////////////
  //////////  indexing  //////////
  ////////////////////////////////



  it should "parse set declaration with simple indexing" in {
    parse("set apples { 1 + 3 .. 10 by 4 };") should be(
      SetDeclaration("apples", indexing = Some(Indexing(SetComprehension(Bin.+(1, 3), 10, 4)))))
  }

  it should "parse set declaration with alias and indexing" in {
    parse("set oranges apples { 1 + 3 .. 10 by 4 };") should be(
      SetDeclaration("oranges", Some("apples"), indexing = Some(Indexing(SetComprehension(Bin.+(1, 3), 10, 4)))))
  }

  it should "parse set declaration with indexing" in {
    parse( """set apples { 1 + 3 .. 10 by 4 , {1, 2, 3}, {"a", "b", "c"} };""") should be(
      SetDeclaration("apples", indexing = Some(Indexing(List(
        SetComprehension(Bin.+(1, 3), 10, 4),
        ExplicitSet(Set[Member](Number(1), Number(2), Number(3))),
        ExplicitSet(Set[Member](StringMember("a"), StringMember("b"), StringMember("c"))))))))
  }

  ///////////////////////////////
  /////////  attributes  /////////
  ///////////////////////////////

  it should "parse set declaration with dimension attribute" in {
    parse("set apples dimen 1;") should be(SetDeclaration("apples", attributes = List(Attribute.Dimension("1"))))
  }

  it should "parse set declaration with within attribute" in {
    parse( """set apples within {"a", "b", "c"} ;""") should be(SetDeclaration("apples",
      attributes = List(Attribute.Within(
        ExplicitSet(Set[Member](StringMember("a"), StringMember("b"), StringMember("c")))))))
  }

  it should "parse set declaration with = attribute" in {
    parse("set numbers = {1, 2, 3};") should be(SetDeclaration("numbers",
      attributes = List(Attribute.FinalSet(
        ExplicitSet(Set[Member](1, 2, 3))))))
  }

  it should "parse set declaration with := attribute the sme as '='" in {
    parse("set numbers := {1, 2, 3};") should be(parse("set numbers = {1, 2, 3};"))
  }

  it should "parse set declaration with default attribute" in {
    parse("set numbers default {1, 2, 3};") should be(SetDeclaration("numbers",
      attributes = List(Attribute.DefaultSet(ExplicitSet(Set[Member](1, 2, 3))))))
  }

  it should "parse set declaration with many attributes" in {
    parse("set arcs dimen 3, within nodes cross nodes, default {1, 2};") should be(SetDeclaration("arcs",
      attributes = List(
        Attribute.Dimension(3),
        Attribute.Within(Sets.Cartesian(SimpleReference("nodes"), SimpleReference("nodes"))),
        Attribute.DefaultSet(ExplicitSet(Set[Member](1, 2))))))
  }

  it should "parse set declaration with alias and attribute" in {
    parse("set apples oranges dimen 1;") should be(SetDeclaration("apples", Some("oranges"),
      attributes = List(Attribute.Dimension(1))))
  }

  it should "parse set declaration with indexing and attribute" in {
    parse("set apples {i in 1 .. 10} dimen 1;") should be(SetDeclaration("apples",
      indexing = Some(Indexing(IndexedSet(List("i"), SetComprehension(1, 10)))),
      attributes = List(Attribute.Dimension(1))))
  }

  it should "parse set declaration with alias and indexing and  attribute" in {
    parse("set apples oranges {i in 1 .. 10} dimen 1;") should be(SetDeclaration("apples", Some("oranges"),
      indexing = Some(Indexing(IndexedSet(List("i"), SetComprehension(1, 10)))),
      attributes = List(Attribute.Dimension(1))))
  }
}
