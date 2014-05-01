package pl.edu.agh.mplt.manipulators

import pl.edu.agh.mplt.parser.declaration.Declaration


trait Manipulator {
  def translateDeclaration(declaration: Declaration): String
}
