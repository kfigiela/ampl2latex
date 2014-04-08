package pl.edu.agh.mplt.parser.declaration.assertion

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression

trait CheckAMPLParser extends JavaTokenParsers {
  def indexing: Parser[Indexing]

  def lexpr: Parser[LogicalExpression]


  def check = "check" ~> (check1 | check2 | check3) <~ ";"

  private def check1 = lexpr ^^ { case lexpr => Assertion(None, lexpr) }

  private def check2 = ":" ~> lexpr ^^ { case lexpr => Assertion(None, lexpr) }

  private def check3 = indexing ~ ":" ~ lexpr ^^ { case ind ~ _ ~ lexpr => Assertion(Some(ind), lexpr) }
}
