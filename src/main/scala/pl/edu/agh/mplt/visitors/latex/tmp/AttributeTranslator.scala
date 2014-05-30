package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.declaration.data.Attribute
import pl.edu.agh.mplt.parser.declaration.data.Attribute._


class AttributeTranslator extends Translator[Attribute] {

   override def apply(node: Attribute): String = node match {
      case Binary   => s""
      case Integer  => s""
      case Symbolic => s""

      case Relation(op, expr) => s"${translateOp(op) } ${(new ExprTranslator)(expr) }"

      case DefaultValue(expr) => s"= {${(new ExprTranslator)(expr) }}"
      case FinalValue(expr)   => s"= {${(new ExprTranslator)(expr) }}"
      case Defined(expr)      => s":= {${(new ExprTranslator)(expr) }}"

      case Inclusion(sexpr)  => s"\\in {${(new SexprTranslator)(sexpr) }}"
      case DefaultSet(sexpr) => s"= {${(new SexprTranslator)(sexpr) }}"
      case FinalSet(sexpr)   => s"= {${(new SexprTranslator)(sexpr) }}"
      case Within(sexpr)     => s"\\subseteq  {${(new SexprTranslator)(sexpr) }}"

      case Dimension(n) => s"\\in \\mathbb{R}^{$n}"

      case Coefficient(_, _, _) => "Unsupported Attribute: coefficiet"
      case Objective(_, _, _)   => "Unsupported Attribute: objective"
      case Cover(_, _)          => "Unsupported Attribute: cover"

      case a => s"Unsupported attribute: $a"
   }
}
