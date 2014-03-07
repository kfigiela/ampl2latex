package AMPL.expressions

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.set.SetExpressionAMPLParser
import pl.edu.agh.mplt.parser.expression.{Number, ExpressionAMPLParser}
import pl.edu.agh.mplt.parser.set.indexing.IndexingAMPLParser
import pl.edu.agh.mplt.parser.logical.LogicalExpressionAMPLParser
import pl.edu.agh.mplt.parser.expression.arithmetic.ArithmeticAMPLParser
import java.lang.Number

class indexingExpressionTest extends FlatSpec with Matchers {


  val parser = new IndexingAMPLParser with SetExpressionAMPLParser with LogicalExpressionAMPLParser with ExpressionAMPLParser with ArithmeticAMPLParser

  def expr = parser.indexing

  def parse(input: String) = parser.parse(expr, input).get

  "Indexing Parsers" should "parser simple indexing expression" in {
    println(parse{"{ A }"})
  }
}
