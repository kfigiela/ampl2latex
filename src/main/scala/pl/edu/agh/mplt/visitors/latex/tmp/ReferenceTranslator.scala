package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.reference.{IndexedReference, SimpleReference, Reference}


class ReferenceTranslator extends Translator[Reference] {
   override def apply(node: Reference): String = node match {
      case SimpleReference(name)          => name
      case IndexedReference(ref, Nil)     => apply(ref)
      case IndexedReference(ref, indexes) =>
         s"${apply(ref) }[${joinWith("][")(indexes.map((new ExprTranslator)(_))) }]"
   }
}
