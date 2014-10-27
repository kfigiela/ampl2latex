package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.phrase.logical._
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.parser.phrase.logical.Logical._
import pl.edu.agh.mplt.parser.phrase.logical.LogicalReduction._
import pl.edu.agh.mplt.parser.phrase.logical.ParenthesizedLogical
import pl.edu.agh.mplt.visitors.translator.Translator


class LexprTranslator extends Translator[LogicalExpression] {
   override def apply(node: LogicalExpression): String = node match {
      case ParenthesizedLogical(lexpr) => s"(${apply(lexpr) })"

      case Inclusion.Member(member, set) =>
         s"${(new MemberTranslator)(member) } \\in {${(new SexprTranslator)(set) }}"
      case Inclusion.Subset(subset, set) =>
         s"${(new SexprTranslator)(subset) } \\subseteq {${(new SexprTranslator)(set) }}"
      case Exclusion.Member(member, set) =>
         s"${(new MemberTranslator)(member) } \\nsubseteq {${(new SexprTranslator)(set) }}"
      case Exclusion.Subset(subset, set) =>
         s"${(new SexprTranslator)(subset) } \\in {${(new SexprTranslator)(set) }}"

      case Not(l)    => s"~ {${apply(l) }"
      case Or(l, r)  => s"${apply(l) } \\vee ${apply(r) }"
      case And(l, r) => s"${apply(l) } \\wedge ${apply(r) }"

      case Forall(indexing, lexpr) =>
         s"${(new IndexingTranslator)(indexing) }: {${(new LexprTranslator)(lexpr) }}"
      case Exists(indexing, lexpr) =>
         s"${(new IndexingTranslator)(indexing) }: \\exists {${(new LexprTranslator)(lexpr) }}"

      case Comparision.<=(l, r) => s"${(new ExprTranslator)(l) } \\le ${(new ExprTranslator)(r) }"
      case Comparision.<(l, r)  => s"${(new ExprTranslator)(l) } < ${(new ExprTranslator)(r) }"
      case Comparision.>=(l, r) => s"${(new ExprTranslator)(l) } \\ge ${(new ExprTranslator)(r) }"
      case Comparision.>(l, r)  => s"${(new ExprTranslator)(l) } > ${(new ExprTranslator)(r) }"
      case Comparision.==(l, r) => s"${(new ExprTranslator)(l) } == ${(new ExprTranslator)(r) }"
      case Comparision.!=(l, r) => s"${(new ExprTranslator)(l) } \\le ${(new ExprTranslator)(r) }"

      case ref: Reference => (new ReferenceTranslator)(ref)
   }
}
