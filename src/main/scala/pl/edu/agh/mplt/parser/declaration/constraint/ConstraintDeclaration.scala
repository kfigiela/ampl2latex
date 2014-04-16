package pl.edu.agh.mplt.parser.declaration.constraint

import pl.edu.agh.mplt.parser.declaration.{PiecewiseLinearTerm, Declaration}
import pl.edu.agh.mplt.parser.formula.set.Indexing


case class ConstraintDeclaration(name: String,
                                 alias: Option[String] = None,
                                 indexing: Option[Indexing] = None,
                                 constraint: ConstraintExpression) extends Declaration


