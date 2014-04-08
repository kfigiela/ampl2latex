package pl.edu.agh.mplt.parser.declaration.assertion

import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression
import pl.edu.agh.mplt.parser.declaration.Declaration


case class Assertion(indexing: Option[Indexing] = None, lexpr: LogicalExpression) extends Declaration
