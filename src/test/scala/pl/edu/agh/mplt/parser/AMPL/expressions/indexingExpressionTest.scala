package pl.edu.agh.mplt.parser.AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.formula.set.{SetExpression, ExplicitSet, SetComprehension, SetExpressionAMPLParser}
import pl.edu.agh.mplt.parser.formula.expression.{Number, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.formula.set.indexing.{Indexing, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.formula.expression.arithmetic.ArithmeticAMPLParser
import pl.edu.agh.mplt.parser.IntercodeImplicits
import pl.edu.agh.mplt.parser.member.MemberAMPLParser

class indexingExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new IndexingAMPLParser with SetExpressionAMPLParser with LogicalExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser with MemberAMPLParser

  def expr = parser.indexing

  def parse(input: String) = parser.parse(expr, input).get

  "Indexing Parsers" should "parse simple indexing expression" in {
    parse(" { { } }") should be(Indexing(List(ExplicitSet())))
    parse("{ 1 .. 10  }") should be(Indexing(List(SetComprehension(1, 10))))
    parse("{ 1 .. 10 by 3  }") should be(Indexing(List(SetComprehension(1, 10, 3))))
  }
}
