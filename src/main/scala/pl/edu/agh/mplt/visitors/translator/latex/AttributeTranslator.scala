package pl.edu.agh.mplt.visitors.translator.latex

import pl.edu.agh.mplt.parser.declaration.data.Attribute
import pl.edu.agh.mplt.parser.declaration.data.Attribute._
import pl.edu.agh.mplt.visitors.translator.Translator


class AttributeTranslator(name:String) extends Translator[Attribute] {

   override def apply(node: Attribute): String = node match {
      case Binary   => s"binary"
      case Integer  => s"int"
      case Symbolic => s"symbol"

      case Relation(op, expr) => s"$name ${translateOp(op) } ${(new ExprTranslator)(expr) }"

      case DefaultValue(expr) => s"$name = {${(new ExprTranslator)(expr) }}"
      case FinalValue(expr)   => s"$name = {${(new ExprTranslator)(expr) }}"
      case Definition(expr)   => s"$name := {${(new ExprTranslator)(expr) }}"

      case Inclusion(sexpr)  => s"$name \\in {${(new SexprTranslator)(sexpr) }}"
      case DefaultSet(sexpr) => s"$name = {${(new SexprTranslator)(sexpr) }}"
      case FinalSet(sexpr)   => s"$name = {${(new SexprTranslator)(sexpr) }}"
      case Membership(sexpr) => s"$name \\subseteq  {${(new SexprTranslator)(sexpr) }}"

      case Dimension(n) => s"$name \\in \\mathbb{R}^{$n}"

      case Coefficient(_, _, _) => "Unsupported Attribute: coefficiet"
      case Objective(_, _, _)   => "Unsupported Attribute: objective"
      case Cover(_, _)          => "Unsupported Attribute: cover"

      case a => throw new Error(s"Unsupported attribute: $a")
   }
}
