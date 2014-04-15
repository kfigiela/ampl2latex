package pl.edu.agh.mplt.parser.declaration

import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.formula.set.Indexing


case class PiecewiseLinearTerm(breakpoints: List[(Option[Indexing], Expression)],
                               slopes: List[(Option[Indexing], Expression)],
                               arguments: (Expression, Option[Expression]))