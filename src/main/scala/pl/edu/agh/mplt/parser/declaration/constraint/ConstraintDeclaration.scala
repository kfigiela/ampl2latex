package pl.edu.agh.mplt.parser.declaration.constraint

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.phrase.set.Indexing


case class ConstraintDeclaration(name: String,
                                 alias: Option[String] = None,
                                 indexing: Option[Indexing] = None,
                                 constraint: ConstraintExpression) extends Declaration


