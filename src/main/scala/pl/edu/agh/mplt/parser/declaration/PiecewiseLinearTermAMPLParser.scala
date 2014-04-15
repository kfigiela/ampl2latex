package pl.edu.agh.mplt.parser.declaration

import scala.util.parsing.combinator.JavaTokenParsers
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.Indexing


trait PiecewiseLinearTermAMPLParser extends JavaTokenParsers {
  def expr: Parser[Expression]

  def indexing: Parser[Indexing]

  def piecewiseLinearTerm :Parser[PiecewiseLinearTerm]= pointsAndSlopes ~ rep1sep(expr, ",") ^^ {
    case (br, sl) ~ (head :: Nil) => PiecewiseLinearTerm(br, sl, (head, None))
    case (br, sl) ~ (h1 :: h2 :: Nil) => PiecewiseLinearTerm(br, sl, (h1, Some(h2)))
  }

   def pointsAndSlopes: Parser[(List[(Option[Indexing], Expression)], List[(Option[Indexing], Expression)])] =
    "<<" ~> exprs ~ ";" ~ exprs <~ ">>" ^^ { case br ~ _ ~ sl => (br, sl) }

   def exprs: Parser[List[(Option[Indexing], Expression)]] =
    rep1sep((indexing ?) ~ expr ^^ { case indOpt ~ expr =>
      (indOpt, expr)
    case _ => throw new Exception("exprs")}, ",")
}
