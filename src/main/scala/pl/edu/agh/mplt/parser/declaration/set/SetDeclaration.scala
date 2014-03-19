package pl.edu.agh.mplt.parser.declaration.set

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.declaration.set.attributes.SetAttribute
import pl.edu.agh.mplt.parser.formula.set.Indexing


case class SetDeclaration(name: String,
                          alias: Option[String] = None,
                          indexing: Option[Indexing] = None,
                          attributes: List[SetAttribute] = List()) extends Declaration
