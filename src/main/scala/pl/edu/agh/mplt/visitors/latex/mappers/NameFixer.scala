package pl.edu.agh.mplt.visitors.latex.mappers

import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.declaration.data.{ParameterDeclaration, VariableDeclaration, SetDeclaration}
import pl.edu.agh.mplt.parser.reference.{Reference, SimpleReference}
import scala.collection.mutable
import pl.edu.agh.mplt.parser.declaration.Declaration

class NameFixer(operations: mutable.Seq[NodeMapper] = Nil) extends NodeMapper(operations) {

  override def mapName(name: String): String = name.replace("_", "\\_")
}
