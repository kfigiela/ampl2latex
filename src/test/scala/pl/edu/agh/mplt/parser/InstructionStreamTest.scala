package pl.edu.agh.mplt.parser

import org.scalatest.{FlatSpec, Matchers}
import pl.edu.agh.mplt.manipulators.LatexTranslator

class InstructionStreamTest extends FlatSpec with Matchers {

  it should "do" in {
    val tr = new LatexTranslator

    val parser: AMPLParser = AMPLParser()

    val str =
      """ var x = (1 + 3) * (x - y) ** 2 / (z ** y ^ 15
        | mod y + abs(atan2(3, y ** x - 5) * ceil(log(x) * log10(y))))
        | + max(1, 2, 3, 4, 5, 6 , 7, oo)
        | * sqrt(13*i)
        | + sum {i in A} (i + 3) +
        | - min(5, 5) ** floor( exp(x*y));""".stripMargin

    val res = parser.parse(str).get

    println(tr.translateDeclaration(res))

  }
}
