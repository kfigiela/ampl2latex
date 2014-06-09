package pl.edu.agh.mplt.parser

import org.scalatest.{FlatSpec, Matchers}
import pl.edu.agh.mplt.ParsedFile
import pl.edu.agh.mplt.visitors.translator.latex.LatexTranslator
import pl.edu.agh.mplt.parser.declaration.Declaration

class InstructionStreamTest extends FlatSpec with Matchers {

   it should "do" in {
      val tr = new LatexTranslator
      val parser: AMPLParser = AMPLParser()

      val v1 =
         """ var v1 = (1 + 3) * (x - y) ** 2 / (z ** y ^ 15
           | mod y + abs(atan2(3, y ** x - 5) * ceil(log(x) * log10(y))))
           | + max(1, 2, 3, 4, 5, 6 , 7, oo)
           | * sqrt(13*i)
           | + sum {i in A, j in B} (i + j) +
           | - min(5, 5) ** floor( exp(x*y));""".stripMargin

      val p1 =
         """ param p1 {j in A: j > 0} < 10, > -10, in {1, 2, 3, e ^ x}, <> v1, default if (v1 > 3) then s1[j, i+1, t*3];"""

      val s1 =
         """set s1 oranges {i in INDICES} dimen 14, default if(7 * 5 < i) then 1..2.3 else 1..3 by 6;"""

      val o1 =
         """minimize Total_Cost:  sum {j in INPUT} (((cost[j] * X[j])));"""

      val c1 =
         """subject to veq418:
           |	7.678464307138575e-5*v418 * v418 + 7.678464307138575e-5*v419 * v419 -
           |	3.055163874676795e-4*atan ( (x418-2.0) / 0.05 )  - 3.055163874676795e-4*atan (
           |	(x419-2.0) / 0.05 )  - 3.055163874676795e-4*atan ( (x418-4.0) / 0.05 )  -
           |	3.055163874676795e-4*atan ( (x419-4.0) / 0.05 )  + 1.0000671865626876*v419 -
           |	0.9999328134373126*v418 - 4.7990401919616074e-4*ua419 -
           |	4.7990401919616074e-4*ua418 - 4.7990401919616074e-4*ub419 -
           |	4.7990401919616074e-4*ub418 + 2.8794241151769645e-4 = 0;""".stripMargin

      val decs2Parse = List(c1)

      val parsedDecs: Stream[Declaration] = decs2Parse.map(parser.parse).map(_.get).toStream

      val file = new ParsedFile(null, null) {
         override lazy val declarations: Stream[Declaration] = parsedDecs
      }
      val res =
         file.translateVerbose
         .map { case (str, ds) => str + ":\n" + ds.reduce(_ + " \\\\ \n" + _) }
         .reduce(_ + _)

      println(res)

   }
}
