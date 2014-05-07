package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.declaration.Declaration


trait Visitor {
  def translateDeclaration(declaration: Declaration): String
}
