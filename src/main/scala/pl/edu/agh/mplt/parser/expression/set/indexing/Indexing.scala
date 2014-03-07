package pl.edu.agh.mplt.parser.expression.set.indexing

import pl.edu.agh.mplt.parser.expression.set.SetExpression
import pl.edu.agh.mplt.parser.expression.Expression
import pl.edu.agh.mplt.parser.logical.LogicalExpression

case class Indexing(sexprs: List[SetExpression],
                    lexpr: Option[LogicalExpression] = None) extends SetExpression