package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.member.{Member, MemberAMPLParser}
import pl.edu.agh.mplt.parser.reference.ReferenceParser
import pl.edu.agh.mplt.parser.declaration.variable.{VariableDeclarationAMPLParser, VariableAttributesAMPLParser}
import pl.edu.agh.mplt.parser.declaration.Attribute
import pl.edu.agh.mplt.parser.reference.SimpleReference
import pl.edu.agh.mplt.parser.declaration.variable.VariableDeclaration
import pl.edu.agh.mplt.parser.formula.set.Indexing
import scala.Some
import pl.edu.agh.mplt.parser.formula.set.IndexedSet

class VarDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new VariableDeclarationAMPLParser with IndexingAMPLParser with SetExpressionAMPLParser
    with ExpressionAMPLParser with LogicalExpressionAMPLParser
    with VariableAttributesAMPLParser with MemberAMPLParser with ReferenceParser with KeywordAMPLParser

  def expr = parser.variableDeclaration

  def parse(input: String) = parser.parse(expr, input).get

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
    parse("var x := 3;") should be(VariableDeclaration("x", attributes = List(Attribute.Initial(3))))
  }

  it should "parse var declaration with default attribute" in {
    parse("var x default 3;") should be(VariableDeclaration("x", attributes = List(Attribute.Default(3))))
  }

  it should "parse var declaration with '=' attribute" in {
    parse("var x = 3;") should be(VariableDeclaration("x", attributes = List(Attribute.Relation("==", 3))))
  }

  it should "parse var declaration with inclusion attribute" in {
    parse("var x in {1, 2, 3};") should be(
      VariableDeclaration("x", attributes = List(Attribute.Inclusion(ExplicitSet(Set[Member](1, 2, 3))))))
  }

  it should "parse multiple attributes" in {
    parse("var x integer, in {1, 2, 3};") should be(
      VariableDeclaration("x", attributes = List(Attribute.Integer, Attribute.Inclusion(ExplicitSet(Set[Member](1, 2, 3))))))
  }

}
