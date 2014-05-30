package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.formula.logical._
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.parser.formula.logical.Logical._
import pl.edu.agh.mplt.parser.formula.logical.LogicalReduction.{Exists, Forall}
import pl.edu.agh.mplt.parser.formula.logical.ParenthesizedLogical


class LexprTranslator extends Translator[LogicalExpression] {
   override def apply(node: LogicalExpression): String = node match {
      case ParenthesizedLogical(lexpr) => s"(${apply(lexpr) })"

      case Inclusion.member(member, set) =>
         s"${(new MemberTranslator)(member) } \\in {${(new SexprTranslator)(set) }}"
      case Inclusion.subset(subset, set) =>
         s"${(new SexprTranslator)(subset) } \\subseteq {${(new SexprTranslator)(set) }}"
      case Exclusion.member(member, set) =>
         s"${(new MemberTranslator)(member) } \\nsubseteq {${(new SexprTranslator)(set) }}"
      case Exclusion.subset(subset, set) =>
         s"${(new SexprTranslator)(subset) } \\in {${(new SexprTranslator)(set) }}"

      case not(l)    => s"~ {${apply (l)}"
      case or(l, r)  => s"${apply(l)} \\vee ${apply(r)}"
      case and(l, r) => s"${apply(l)} \\wedge ${apply(r)}"

      case Forall(indexing, lexpr) =>
                  s"${(new IndexingTranslator)(indexing) }: {${(new LexprTranslator)(lexpr) }}"
      case Exists(indexing, lexpr) =>
         s"${(new IndexingTranslator)(indexing) }: \\exists {${(new LexprTranslator)(lexpr) }}"

      case Comparision.<=(l, r) => s"${(new ExprTranslator)(l)} \\le ${(new ExprTranslator)(r)}"
      case Comparision.<(l, r)  => s"${(new ExprTranslator)(l)} < ${(new ExprTranslator)(r)}"
      case Comparision.>=(l, r) => s"${(new ExprTranslator)(l)} \\ge ${(new ExprTranslator)(r)}"
      case Comparision.>(l, r)  => s"${(new ExprTranslator)(l)} > ${(new ExprTranslator)(r)}"
      case Comparision.==(l, r) => s"${(new ExprTranslator)(l)} == ${(new ExprTranslator)(r)}"
      case Comparision.!=(l, r) => s"${(new ExprTranslator)(l)} \\le ${(new ExprTranslator)(r)}"

      case ref: Reference => (new ReferenceTranslator)(ref)
   }
}
