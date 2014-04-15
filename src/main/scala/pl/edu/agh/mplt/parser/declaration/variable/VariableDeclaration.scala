package pl.edu.agh.mplt.parser.declaration.variable

import pl.edu.agh.mplt.parser.declaration.{PiecewiseLinearTerm, VariableAttribute, Declaration}
import pl.edu.agh.mplt.parser.formula.set.Indexing


case class VariableDeclaration(name: String,
                               alias: Option[String] = None,
                               indexing: Option[Indexing] = None,
                               attributes: List[VariableAttribute] = List(),
                               piecewiseLinearTerms: Option[PiecewiseLinearTerm] = None) extends Declaration