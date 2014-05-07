package pl.edu.agh.mplt.parser

import org.scalatest.{FlatSpec, Matchers}
import pl.edu.agh.mplt.visitors.LatexTranslator

class InstructionStreamTest extends FlatSpec with Matchers {

  it should "do" in {
    val tr = new LatexTranslator
    val parser: AMPLParser = AMPLParser()

    val v1=
      """ var v1 integer, <= 54, = (1 + 3) * (x - y) ** 2 / (z ** y ^ 15
        | mod y + abs(atan2(3, y ** x - 5) * ceil(log(x) * log10(y))))
        | + max(1, 2, 3, 4, 5, 6 , 7, oo)
        | * sqrt(13*i)
        | + sum {i in A, j in B} (i + j) +
        | - min(5, 5) ** floor( exp(x*y));""".stripMargin

    val v2 =
    """ param v2 {j in A: j > 0} < 10, > -10, in {1, 2, 3, e ^ x}, <> v1, default if (v1 > 3) then 5;"""

    val s1 =
    """set apples oranges {i in INDICES} dimen 14, default if(7 * 5 < i) then 1..2.3 else 1..3 by 6;"""

    val decs = List(v1, v2, s1)

    val declarations = decs.map(parser.parse).map(_.get).map(tr.translateDeclaration).reduce(_ + " \\\\ \n"  + _)

    println(declarations)

//    val res = parser.parse(v2).get
//
//    println(tr.translateDeclaration(res))

  }
}
