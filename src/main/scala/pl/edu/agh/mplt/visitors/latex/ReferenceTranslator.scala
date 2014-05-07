package pl.edu.agh.mplt.visitors.latex

import pl.edu.agh.mplt.parser.reference.{IndexedReference, SimpleReference, Reference}
import pl.edu.agh.mplt.parser.formula.expression.Expression
import pl.edu.agh.mplt.parser.ASTNode


trait ReferenceTranslator {
  def translateExpression(expr: Expression): String

  def translateRef(ref: Reference): String = ref match {
    case SimpleReference(name) => name
    case IndexedReference(name, indexes) => s"$name${reduce("[", "]")("][")(indexes, translateExpression)}"

  }

  def reduce[A <: ASTNode](begin: String, end: String)(delim: String)(list: Traversable[A], f: A => String): String

  private def translateIndexes(indexes: List[Expression]): String = indexes match {
    case Nil => ""
    case hd :: tl => (translateExpression(hd) /: tl)(_ + "][" + translateExpression(_))
  }
}
