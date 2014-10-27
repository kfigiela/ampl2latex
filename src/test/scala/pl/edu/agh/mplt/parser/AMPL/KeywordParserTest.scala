package pl.edu.agh.mplt.parser.AMPL

import pl.edu.agh.mplt.parser.{IntercodeImplicits, KeywordAMPLParser}
import org.scalatest.{Matchers, FlatSpec}


class KeywordParserTest extends FlatSpec with Matchers with IntercodeImplicits {
  val parser = new KeywordAMPLParser {}

  def expr = parser.nonKeyword

  def parse(input: String) = parser.parse(expr, input).get

  "Keyword parser" should "parse nonKeywords" in {
    parse("INPUT")
    intercept[RuntimeException] {
      parse("IN")
    }
  }
}
