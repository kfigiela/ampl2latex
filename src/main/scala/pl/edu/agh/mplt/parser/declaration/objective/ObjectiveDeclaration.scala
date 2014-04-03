package pl.edu.agh.mplt.parser.declaration.objective

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.expression.Expression

trait ObjectiveDeclaration extends Declaration

case class Minimize(name: String,
                    alias: Option[String] = None,
                    indexing: Option[Indexing] = None,
                    expression: Expression) extends ObjectiveDeclaration

case class Maximize(name: String,
                    alias: Option[String] = None,
                    indexing: Option[Indexing] = None,
                    expression: Expression) extends ObjectiveDeclaration