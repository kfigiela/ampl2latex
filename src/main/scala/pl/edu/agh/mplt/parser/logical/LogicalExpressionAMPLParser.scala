package pl.edu.agh.mplt.parser.logical

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.logical.LogicalExpression


trait LogicalExpressionAMPLParser extends JavaTokenParsers {
  def lexpr: Parser[LogicalExpression] = "lexpr" ^^ (_ => new LogicalExpression {})
}
