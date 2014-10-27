package pl.edu.agh.mplt.parser.declaration.data

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.phrase.set.Indexing


trait DataDeclaration extends Declaration {
   def name: String

   def alias: Option[String]

   def indexing: Option[Indexing]

   def attributes: List[Attribute]
}

case class ParameterDeclaration(name: String,
                                alias: Option[String] = None,
                                indexing: Option[Indexing] = None,
                                attributes: List[Attribute] = List()) extends DataDeclaration

case class VariableDeclaration(name: String,
                               alias: Option[String] = None,
                               indexing: Option[Indexing] = None,
                               attributes: List[Attribute] = List()) extends DataDeclaration

case class SetDeclaration(name: String,
                          alias: Option[String] = None,
                          indexing: Option[Indexing] = None,
                          attributes: List[Attribute] = List()) extends DataDeclaration