package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.parser.declaration.Declaration


trait Translator[A <: ASTNode] extends TmpVisitor[A, String] {
  override def visit(declaration: Declaration): String = ???
}
