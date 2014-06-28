package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.reference.{IndexedReference, SimpleReference, Reference}
import pl.edu.agh.mplt.visitors.translator.Translator


class ReferenceTranslator extends Translator[Reference] {
   override def apply(node: Reference): String = node match {
      case SimpleReference(name)          => name
      case IndexedReference(ref, Nil)     => apply(ref)
      case IndexedReference(ref, indexes) =>
         s"${apply(ref) }_{${joinWith(",")(indexes.map((new ExprTranslator)(_))) }}"
   }
}
