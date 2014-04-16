package pl.edu.agh.mplt.parser.declaration.datatype

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.formula.set.Indexing


trait DatatypeDeclaration extends Declaration

case class ParameterDeclaration(name: String,
                                alias: Option[String] = None,
                                indexing: Option[Indexing] = None,
                                attributes: List[Attribute] = List()) extends DatatypeDeclaration

case class VariableDeclaration(name: String,
                               alias: Option[String] = None,
                               indexing: Option[Indexing] = None,
                               attributes: List[Attribute] = List()) extends DatatypeDeclaration

case class SetDeclaration(name: String,
                          alias: Option[String] = None,
                          indexing: Option[Indexing] = None,
                          attributes: List[Attribute] = List()) extends DatatypeDeclaration