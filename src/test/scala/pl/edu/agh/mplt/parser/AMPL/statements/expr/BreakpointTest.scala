package pl.edu.agh.mplt.parser.AMPL.statements.expr

import org.scalatest.{Matchers, FlatSpec}
import pl.edu.agh.mplt.parser.{AMPLParser, IntercodeImplicits}


class BreakpointTest extends FlatSpec with Matchers with IntercodeImplicits {
   val parser = AMPLParser()

   def expr = parser.objectiveDeclaration

   def parse(input: String) = parser.parse(expr, input)

   it should "parse complex objective with breakpoint" in {
      parse(
         """
           |maximize Total_Profit:
           |   sum {p in PROD, t in 1..T} (revenue[p,t]*Sell[p,t] -
           |      prodcost[p]*Make[p,t] - invcost[p]*Inv[p,t])
           | - sum {t in 1..T} <<avail_min[t]; 0,time_penalty[t]>> Use[t]
           | ;
         """.stripMargin) match {
         case parser.Success(_, _) =>
         case _                    => throw new Exception
      }
   }

   it should "parse breakpoint expression with two expresssion at the end" in {
      parse(
         """
           | maximize Total_Profit:
           | sum {p in PROD, t in 1..T} (revenue[p,t]*Sell[p,t] -
           | prodcost[p]*Make[p,t] - invcost[p]*Inv[p,t]) -
           | sum {t in 1..T} <<avail_min[t]; 0,time_penalty[t]>> Use[t] - sum {p in PROD, t in 1..T}
           | <<commit[p,t]; -100000,0>> (Sell[p,t],commit[p,t]);
         """.stripMargin) match {
         case parser.Success(_, _) =>
         case _                    => throw new Exception
      }
   }
}