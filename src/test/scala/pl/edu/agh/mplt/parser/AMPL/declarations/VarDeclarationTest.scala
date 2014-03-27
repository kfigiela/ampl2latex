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

class VarDeclarationTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new SetDeclarationAMPLParser with IndexingAMPLParser with SetExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser with LogicalExpressionAMPLParser with SetAttributesAMPLParser with MemberAMPLParser with ReferenceParser

  def expr = parser.setDeclaration

  def parse(input: String) = parser.parse(expr, input).get

  "Variable declaration parser" should "parse simple var declaration" in {
    parse("var x;")
  }

  it should "parse var declaration with an alias" in {
    parse("var x y;")
  }

  ////////////////////////////////
  //////////  indexing  //////////
  ////////////////////////////////

  it should "parse var declaration with indexing" in {
    parse("var x {i in A};")
  }

  ///////////////////////////////
  /////////  attributes  /////////
  ///////////////////////////////

  it should "parse var declaration with binary attribute" in {
    parse("var x binary;")
  }

  it should "parse var declaration with integer attribute" in {
    parse("var x integer;")
  }

  it should "parse var declaration with symbolic attribute" in {
    parse("var x symbolic;")
  }

  it should "parse var declaration with '>=' attribute" in {
    parse("var x >= 3;")
  }

  it should "parse var declaration with '<=' attribute" in {
    parse("var x <= 3;")
  }

  it should "parse var declaration with ':=' attribute" in {
    parse("var x := 3;")
  }

  it should "parse var declaration with  attribute" in {
    parse("var x default 3;")
  }

  it should "parse var declaration with '=' attribute" in {
    parse("var x = 3;")
  }

  it should "parse var declaration with  attribute" in {
    parse("var x in {1, 2, 3};")
  }

  it should "parse multiple attributes" in {
    parse("var x integer, in {1, 2, 3};")
  }

}
