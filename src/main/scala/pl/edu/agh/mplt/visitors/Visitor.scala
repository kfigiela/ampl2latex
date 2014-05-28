package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.parser.ASTNode


trait Visitor[A] {
  def visit(declaration: Declaration): A

  def apply(node:ASTNode):A
}
