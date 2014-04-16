package pl.edu.agh.mplt.parser.declaration.param

import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.declaration.{Attribute, Declaration}


case class ParameterDeclaration(name: String,
                                alias: Option[String] = None,
                                indexing: Option[Indexing] = None,
                                attributes: List[Attribute] = List()) extends Declaration
