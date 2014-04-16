package pl.edu.agh.mplt.parser.reference

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Expression

trait ReferenceParser extends JavaTokenParsers {

  def refName: Parser[String]

  def expr: Parser[Expression]

  def reference: Parser[Reference] = indexedReference | simpleReference

  private def simpleReference = refName ^^ SimpleReference

  private def indexedReference = simpleReference ~ "[" ~ rep1sep(expr, ",") <~ "]" ^^ {
    case ref ~ _ ~ expr => IndexedReference(ref, expr)
  }

}
