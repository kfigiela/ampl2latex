package pl.edu.agh.mplt.parser.formula.set.indexing

import pl.edu.agh.mplt.parser.formula.logical.LogicalExpression
import pl.edu.agh.mplt.parser.formula.set.SetExpression

case class Indexing(sexprs: List[SetExpression],
                    lexpr: Option[LogicalExpression] = None) extends SetExpression

