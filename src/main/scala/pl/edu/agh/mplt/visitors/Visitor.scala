package pl.edu.agh.mplt.visitors

import pl.edu.agh.mplt.parser.ASTNode

trait Visitor[A <: ASTNode, B]  {
  def apply(node: A): B
}
