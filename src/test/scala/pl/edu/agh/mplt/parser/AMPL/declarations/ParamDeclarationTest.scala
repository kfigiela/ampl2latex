package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.phrase.set._
import pl.edu.agh.mplt.parser.member.SetMember
import pl.edu.agh.mplt.parser.reference.SimpleReference
import pl.edu.agh.mplt.parser.phrase.set.Indexing
import scala.Some
import pl.edu.agh.mplt.parser.phrase.set.IndexedSet
import pl.edu.agh.mplt.parser.declaration.data.{ParameterDeclaration, Attribute}


class ParamDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = AMPLParser()

  def expr = parser.datatypeDeclaration

  def parse(input: String) = parser.parse(expr, input).get

  "Parameter declaration parser" should "parse simple parameter declaration" in {
    parse("param x;") should be(ParameterDeclaration("x"))
  }

  it should "parse parameter declaration with an alias" in {
    parse("param x y;") should be(ParameterDeclaration("x", Some("y")))
  }

  ////////////////////////////////
  //////////  indexing  //////////
  ////////////////////////////////

  it should "parse parameter declaration with indexing" in {
    parse("param x {i in A};") should be(
      ParameterDeclaration("x", indexing = Some(Indexing(IndexedSet(List("i"), SimpleReference("A"))))))
  }

  ///////////////////////////////
  /////////  attributes  /////////
  ///////////////////////////////

  it should "parse parameter declaration with binary attribute" in {
    parse("param x binary;") should be(ParameterDeclaration("x", attributes = List(Attribute.Binary)))
  }

  it should "parse parameter declaration with integer attribute" in {
    parse("param x integer;") should be(ParameterDeclaration("x", attributes = List(Attribute.Integer)))
  }

  it should "parse parameter declaration with symbolic attribute" in {
    parse("param x symbolic;") should be(ParameterDeclaration("x", attributes = List(Attribute.Symbolic)))
  }

  it should "parse parameter declaration with default attribute" in {
    parse("param x default 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.DefaultValue(3))))
  }

  it should "parse parameter declaration with  attribute" in {
    parse("param x in {1, 2, 3};") should be(
      ParameterDeclaration("x", attributes = List(Attribute.Inclusion(ExplicitSet(Set[SetMember](1, 2, 3))))))
  }

  it should "parse multiple attributes" in {
    parse("param x integer, in {1, 2, 3};") should be(
      ParameterDeclaration("x", attributes = List(Attribute.Integer, Attribute.Inclusion(ExplicitSet(Set[SetMember](1, 2,
        3))))))
  }

  it should "parse parameter declaration with '<' attribute" in {
    parse("param x < 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.Relation("<", 3))))
  }

  it should "parse parameter declaration with '<=' attribute" in {
    parse("param x <= 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.Relation("<=", 3))))
  }

  it should "parse parameter declaration with '>' attribute" in {
    parse("param x > 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.Relation(">", 3))))
  }

  it should "parse parameter declaration with '>=' attribute" in {
    parse("param x >= 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.Relation(">=", 3))))
  }

  it should "parse parameter declaration with '==' attribute" in {
    parse("param x == 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.Relation("==", 3))))
  }

  it should "parse parameter declaration with '=' attribute" in {
    parse("param x = 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.FinalValue(3))))
  }

  it should "parse parameter declaration with '!=' attribute" in {
    parse("param x != 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.Relation("!=", 3))))
  }

  it should "parse parameter declaration with '<>' attribute" in {
    parse("param x <> 3;") should be(ParameterDeclaration("x", attributes = List(Attribute.Relation("!=", 3))))
  }


}