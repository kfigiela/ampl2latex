package pl.edu.agh.mplt.parser.AMPL.declarations

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.declaration.set.{SetAttributesAMPLParser, SetDeclarationAMPLParser}
import pl.edu.agh.mplt.parser.formula.set.{SetExpressionAMPLParser, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.formula.expression.ExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.ArithmeticAMPLParser
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.reference.ReferenceParser


class ParamDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new SetDeclarationAMPLParser with IndexingAMPLParser with SetExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser with LogicalExpressionAMPLParser with SetAttributesAMPLParser with MemberAMPLParser with ReferenceParser

  def expr = parser.declaration

  def parse(input: String) = parser.parse(expr, input).get

  "Parameter declaration parser" should "parse simple var declaration" in {
    parse("param x;")
  }

  it should "parse var declaration with an alias" in {
    parse("param x y;")
  }

  ////////////////////////////////
  //////////  indexing  //////////
  ////////////////////////////////

  it should "parse var declaration with indexing" in {
    parse("param x {i in A};")
  }

  ///////////////////////////////
  /////////  attributes  /////////
  ///////////////////////////////

  it should "parse var declaration with binary attribute" in {
    parse("param x binary;")
  }

  it should "parse var declaration with integer attribute" in {
    parse("param x integer;")
  }

  it should "parse var declaration with symbolic attribute" in {
    parse("param x symbolic;")
  }

  it should "parse var declaration with default attribute" in {
    parse("param x default 3;")
  }

  it should "parse var declaration with  attribute" in {
    parse("param x in {1, 2, 3};")
  }

  it should "parse multiple attributes" in {
    parse("param x integer, in {1, 2, 3};")
  }

  it should "parse var declaration with '<' attribute" in {
    parse("param x < 3;")
  }

  it should "parse var declaration with '<=' attribute" in {
    parse("param x <= 3;")
  }

  it should "parse var declaration with '>' attribute" in {
    parse("param x > 3;")
  }

  it should "parse var declaration with '>=' attribute" in {
    parse("param x >= 3;")
  }

  it should "parse var declaration with '==' attribute" in {
    parse("param x == 3;")
  }

  it should "parse var declaration with '=' attribute" in {
    parse("param x = 3;")
  }

  it should "parse var declaration with '!=' attribute" in {
    parse("param x != 3;")
  }

  it should "parse var declaration with '<>' attribute" in {
    parse("param x <> 3;")
  }


}