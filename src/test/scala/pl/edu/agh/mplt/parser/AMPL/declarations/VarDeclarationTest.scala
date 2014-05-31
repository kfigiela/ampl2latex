package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.phrase.set._
import pl.edu.agh.mplt.parser.member.SetMember
import pl.edu.agh.mplt.parser.reference.SimpleReference
import pl.edu.agh.mplt.parser.phrase.set.Indexing
import scala.Some
import pl.edu.agh.mplt.parser.phrase.set.IndexedSet
import pl.edu.agh.mplt.parser.declaration.data.{VariableDeclaration, Attribute}

class VarDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = AMPLParser()

  def expr = parser.datatypeDeclaration

  def parse(input: String) = parser.parseAll(expr, input).get

  "Variable declaration parser" should "parse simple var declaration" in {
    parse("var x;") should be(VariableDeclaration("x"))
  }

  it should "parse var declaration with an alias" in {
    parse("var x y;") should be(VariableDeclaration("x", Some("y")))
  }

  ////////////////////////////////
  //////////  indexing  //////////
  ////////////////////////////////

  it should "parse var declaration with indexing" in {
    parse("var x {i in A};") should be(
      VariableDeclaration("x", indexing = Some(Indexing(List(IndexedSet(List("i"), SimpleReference("A")))))))
  }

  ///////////////////////////////
  /////////  attributes  /////////
  ///////////////////////////////

  it should "parse var declaration with binary attribute" in {
    parse("var x binary;") should be(VariableDeclaration("x", attributes = List(Attribute.Binary)))
  }

  it should "parse var declaration with integer attribute" in {
    parse("var x integer;") should be(VariableDeclaration("x", attributes = List(Attribute.Integer)))
  }

  it should "parse var declaration with symbolic attribute" in {
    parse("var x symbolic;") should be(VariableDeclaration("x", attributes = List(Attribute.Symbolic)))
  }

  it should "parse var declaration with '>=' attribute" in {
    parse("var x >= 3;") should be(VariableDeclaration("x", attributes = List(Attribute.Relation(">=", 3))))
  }

  it should "parse var declaration with '<=' attribute" in {
    parse("var x <= 3;") should be(VariableDeclaration("x", attributes = List(Attribute.Relation("<=", 3))))
  }

  it should "parse var declaration with ':=' attribute" in {
    parse("var x := 3;") should be(VariableDeclaration("x", attributes = List(Attribute.FinalValue(3))))
  }

  it should "parse var declaration with default attribute" in {
    parse("var x default 3;") should be(VariableDeclaration("x", attributes = List(Attribute.DefaultValue(3))))
  }

  it should "parse var declaration with '=' attribute" in {
    parse("var x = 3;") should be(VariableDeclaration("x", attributes = List(Attribute.FinalValue(3))))
  }

  it should "parse var declaration with inclusion attribute" in {
    parse("var x in {1, 2, 3};") should be(
      VariableDeclaration("x", attributes = List(Attribute.Inclusion(ExplicitSet(Set[SetMember](1, 2, 3))))))
  }

  it should "parse multiple attributes" in {
    parse("var x integer, in {1, 2, 3};") should be(
      VariableDeclaration("x",
        attributes = List(Attribute.Integer, Attribute.Inclusion(ExplicitSet(Set[SetMember](1, 2, 3))))))
  }

  it should "parse coeff attribute" in {
    parse("var x coeff y 1;") should be(VariableDeclaration("x", attributes = List(Attribute.Coefficient(None, "y",
      1))))
  }

  it should "parse coeff attribute with indexing" in {
    parse("var x coeff {A} y 1;") should be(VariableDeclaration(
      "x",
      attributes = List(Attribute.Coefficient(Some(Indexing(List(SimpleReference("A")))), "y", 1))))
  }

  it should "parse cover attribute" in {
    parse("var x cover y;") should be(VariableDeclaration("x", attributes = List(Attribute.Cover(None, "y"))))
  }

  it should "parse objective attribute" in {
    parse("var x obj y 1;") should be(VariableDeclaration("x", attributes = List(Attribute.Objective(None, "y", 1))))
  }

  it should "parse objective attribute with indexing" in {
    parse("var x obj {A} y 1;") should be(VariableDeclaration(
      "x",

      attributes = List(Attribute.Objective(Some(Indexing(List(SimpleReference("A")))), "y", 1))))
  }

}
