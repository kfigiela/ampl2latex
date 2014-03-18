package pl.edu.agh.mplt.parser.set.indexing

import pl.edu.agh.mplt.parser.logical.LogicalExpression
import pl.edu.agh.mplt.parser.set.SetExpression

case class Indexing(sexprs: List[SetExpression],
                    lexpr: Option[LogicalExpression] = None) extends SetExpression

