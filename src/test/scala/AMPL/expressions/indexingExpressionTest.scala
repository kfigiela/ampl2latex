package AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.expression.set.{SetExpression, ExplicitSet, SetComprehension, SetExpressionAMPLParser}
import pl.edu.agh.mplt.parser.expression.{Number, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.expression.set.indexing.{Indexing, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.expression.arithmetic.ArithmeticAMPLParser
import pl.edu.agh.mplt.parser.expression.variable.Variable

class indexingExpressionTest extends FlatSpec with Matchers {
  implicit def intToString(i: Int): String = i.toString

  implicit def intToNumber(i: Int): Number = Number(i.toString)

  implicit def sexprToind(sexprs: SetExpression*): Indexing = Indexing(sexprs.toList)

  val parser = new IndexingAMPLParser with SetExpressionAMPLParser with LogicalExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser

  def expr = parser.indexing

  def parse(input: String) = parser.parse(expr, input).get

  "Indexing Parsers" should "parser simple indexing expression" in {
    parse(" { { } }") should be(Indexing(List(ExplicitSet())))
    parse("{ 1 .. 10  }") should be(Indexing(List(SetComprehension(1, 10))))
    parse("{ 1 .. 10 by 3  }") should be(Indexing(List(SetComprehension(1, 10, 3))))
  }
}
