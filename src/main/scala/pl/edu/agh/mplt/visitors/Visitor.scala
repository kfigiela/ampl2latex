package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.declaration.Declaration


trait Visitor[A] {
  def visit(declaration: Declaration): A
}
