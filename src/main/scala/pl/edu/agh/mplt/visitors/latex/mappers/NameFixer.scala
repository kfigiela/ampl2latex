package pl.edu.agh.mplt.visitors.latex.mappers

import pl.edu.agh.mplt.visitors.NodeMapper
import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.declaration.data.{ParameterDeclaration, VariableDeclaration, SetDeclaration}
import pl.edu.agh.mplt.parser.reference.SimpleReference

class NameFixer(val operations: List[NodeMapper] = Nil) extends NodeMapper {

  override def apply(node: ASTNode): ASTNode = node match {
    case SimpleReference(name) => SimpleReference(fix(name))
    case set@SetDeclaration(name, _, _, _)           => set.copy(name = fix(name))
    case param@ParameterDeclaration(name, _, _, _)   => param.copy(name = fix(name))
    case variable@VariableDeclaration(name, _, _, _) => variable.copy(name = fix(name))
  }

  private def fix(name:String):String = name.replace("_", "\\_")
}
