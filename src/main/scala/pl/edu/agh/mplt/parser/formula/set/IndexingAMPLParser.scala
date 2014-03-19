package pl.edu.agh.mplt.parser.formula.set

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression

trait IndexingAMPLParser extends JavaTokenParsers {
  def lexpr: Parser[LogicalExpression]

  def sexpr: Parser[SetExpression]

  def indexing: Parser[Indexing] = "{" ~> sexprList <~ "}" ^^ { case sexprs => Indexing(sexprs)} | logicalIndexing

  private def indexedSet: Parser[IndexedSet] =
    "[a-zA-Z]\\w*".r ~ "in" ~ sexpr ^^ { case i ~ _ ~ s => IndexedSet(List(i), s)} |
      "(" ~> rep1sep("[a-zA-Z]\\w*".r, ",") ~ ")" ~ "in" ~ sexpr ^^ { case is ~ _ ~ _ ~ s => IndexedSet(is, s)}

  private def sexprList: Parser[List[SetExpression]] = rep1sep(indexedSet | sexpr, ",")

  private def logicalIndexing = "{" ~> sexprList ~ ":" ~ lexpr <~ "}" ^^ { case sexprs ~ _ ~ lexpr => Indexing(sexprs, Some(lexpr))}
}
