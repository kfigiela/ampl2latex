package pl.edu.agh.mplt.parser.declaration.variable

import pl.edu.agh.mplt.parser.declaration.{VariableAttribute, Declaration}
import pl.edu.agh.mplt.parser.formula.set.Indexing


case class VariableDeclaration(name: String,
                               alias: Option[String] = None,
                               indexing: Option[Indexing] = None,
                               attributes: List[VariableAttribute] = List()) extends Declaration