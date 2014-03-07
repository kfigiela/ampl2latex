package pl.edu.agh.mplt.parser.expression.set.indexing

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.expression.set.{SetExpressionWithDummyMember, SetExpression}
import pl.edu.agh.mplt.parser.logical.LogicalExpression
import pl.edu.agh.mplt.parser.expression.Expression

trait IndexingAMPLParser extends JavaTokenParsers {
  def lexpr: Parser[LogicalExpression]

  def sexpr: Parser[SetExpression]

  def indexing: Parser[Indexing] = "{" ~> sexprList <~ "}" ^^ { case sexprs => Indexing(sexprs)} | logicalIndexing

  def logicalIndexing = "{" ~> sexprList ~ ":" ~ lexpr <~ "}" ^^ { case sexprs ~ _ ~ lexpr => Indexing(sexprs, Some(lexpr))}

  def sexprList: Parser[List[SetExpression]] = rep1sep(sexpr, ",") | sexprWithDummy ^^ { case e => List(e)}

  def sexprWithDummy: Parser[SetExpression] = "[a-zA-Z]\\w*".r ~ "in" ~ sexpr ^^ { case id ~ _ ~ sexpr => SetExpressionWithDummyMember(id, sexpr)}
}
