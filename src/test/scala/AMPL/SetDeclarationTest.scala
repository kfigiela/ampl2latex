package AMPL

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.declaration.set.SetDeclarationParser
import pl.edu.agh.mplt.parser.declaration.set.attributes.SetAttribute


class SetDeclarationTest extends FlatSpec with Matchers {

  val parser = new SetDeclarationParser {}

  def expr = parser.declaration

  def parse(input: String) = parser.parse(expr, input).get

  "Set declaration parser" should "parse simple set declaration" in {
    parse("set nodes;") should be(Set("nodes"))
  }

  it should "parse set declaration with simple attribute" in {
    //    parse("set arcs within nodes cross nodes;") should be
    //    (SetDeclarationTest.super("arcs", attributes = List(SetAttribute.is(SetExpression.cross(Variable("nodes"), Variable("nodes"))))))
  }

}
