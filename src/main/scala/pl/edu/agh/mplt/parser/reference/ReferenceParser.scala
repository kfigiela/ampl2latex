package pl.edu.agh.mplt.parser.reference

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Expression

trait ReferenceParser extends JavaTokenParsers {

  def nonKeyword: Parser[String]

  def expr: Parser[Expression]

  def reference: Parser[Reference] = indexedReference | simpleReference

  private def simpleReference = nonKeyword ^^ SimpleReference

  private def indexedReference = simpleReference ~ "[" ~ expr <~ "]" ^^ {
    case ref ~ _ ~ expr => IndexedReference(ref, expr)
  }

}
