package pl.edu.agh.mplt.parser.AMPL.statements.expr

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.phrase.expression._
import pl.edu.agh.mplt.parser.{KeywordAMPLParser, IntercodeImplicits}
import pl.edu.agh.mplt.parser.reference.{IndexedReference, ReferenceParser, SimpleReference}
import pl.edu.agh.mplt.parser.phrase.set.{SetExpressionAMPLParser, IndexingAMPLParser}
import pl.edu.agh.mplt.parser.phrase.logical.{Comparision, LogicalExpressionAMPLParser}
import pl.edu.agh.mplt.parser.member.MemberAMPLParser
import pl.edu.agh.mplt.parser.phrase.expression.Number
import pl.edu.agh.mplt.parser.phrase.expression.ExpressionIf

class ExpressionTest extends FlatSpec with Matchers with IntercodeImplicits {
   val parser = new ReferenceParser with KeywordAMPLParser with ExpressionAMPLParser with IndexingAMPLParser
   with LogicalExpressionAMPLParser with SetExpressionAMPLParser with MemberAMPLParser

   def expr = parser.expr

   def parse(input: String) = parser.parseAll(expr, input).get

   "Expression Parser" should "parse numbers" in {
      parse("1") should be(Number(1))
      parse("1.1") should be(Number("1.1"))
      parse("0.0") should be(Number("0.0"))
      parse("-3.16") should be(Unary.-(Number("3.16")))
      parse("-3.16e10") should be(Unary.-(Number("3.16e10")))
   }

   it should "parse variable" in {
      parse("A") should be(SimpleReference("A"))
      parse("AlaMaKota") should be(SimpleReference("AlaMaKota"))
      parse("12") should not be SimpleReference("12")
   }

   it should "parse conditional expression" in {
      parse("if 1 == 1 then 2") should be(ExpressionIf(Comparision.==(1, 1), 2))
   }

   it should "parse conditional expression with else" in {
      parse("if 1 == 1 then 2 else 3") should be(ExpressionIf(Comparision.==(1, 1), 2, 3))
   }

   it should "parse chained conditional expressions" in {
      parse("if 1 == 1 then if 1 == 1 then 2 else 3") should be(ExpressionIf(Comparision.==(Number(1), Number(1)),
         ExpressionIf(Comparision.==(Number(1), Number(1)),
            Number(2),
            Number(3))))
   }

   it should "parse chained conditional expressions with two elses" in {
      parse("if 1 == 1 then if 1 == 1 then 1 else 2 else 3") should be(ExpressionIf(Comparision
      .==(Number(1), Number(1)),
         ExpressionIf(Comparision.==(Number(1), Number(1)),
            Number(1),
            Number(2)),
         Number(3)))
   }

   it should "parse function call" in {
      parse("max(1, 2, 3)") should be(FunctionCall("max", List[Expression](1, 2, 3)))
   }

   it should "parse more function calls" in {
      parse("floor(time_quantum[i,s]/unit_time[i,s])") should be(
         FunctionCall("floor", List[Expression](
            Bin./(IndexedReference("time_quantum", List[Expression]("i", "s")),
               IndexedReference("unit_time", List[Expression]("i", "s"))))))
   }

}
