package pl.edu.agh.mplt.visitors.latex

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.declaration.Declaration

trait TmpVisitor[A <: ASTNode, B] {
  def visit(declaration: Declaration): B

  def apply(node: A): B
}
