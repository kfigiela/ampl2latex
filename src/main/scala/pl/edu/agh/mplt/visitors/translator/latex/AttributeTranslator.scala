package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.data.Attribute
import pl.edu.agh.mplt.parser.declaration.data.Attribute._
import pl.edu.agh.mplt.visitors.translator.Translator


class AttributeTranslator extends Translator[Attribute] {

   override def apply(node: Attribute): String = node match {
      case Binary   => s"binary"
      case Integer  => s"int"
      case Symbolic => s"symbol"

      case Relation(op, expr) => s"${translateOp(op) } ${(new ExprTranslator)(expr) }"

      case DefaultValue(expr) => s"= {${(new ExprTranslator)(expr) }}"
      case FinalValue(expr)   => s"= {${(new ExprTranslator)(expr) }}"
      case Definition(expr)   => s":= {${(new ExprTranslator)(expr) }}"

      case Inclusion(sexpr)  => s"\\in {${(new SexprTranslator)(sexpr) }}"
      case DefaultSet(sexpr) => s"= {${(new SexprTranslator)(sexpr) }}"
      case FinalSet(sexpr)   => s"= {${(new SexprTranslator)(sexpr) }}"
      case Membership(sexpr) => s"\\subseteq  {${(new SexprTranslator)(sexpr) }}"

      case Dimension(n) => s"\\in \\mathbb{R}^{$n}"

      case Coefficient(_, _, _) => "Unsupported Attribute: coefficiet"
      case Objective(_, _, _)   => "Unsupported Attribute: objective"
      case Cover(_, _)          => "Unsupported Attribute: cover"

      case a => throw new Error(s"Unsupported attribute: $a")
   }
}
