package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.member._


class MemberTranslator extends Translator[Member] {
   override def apply(node: Member): String = node match {
      case MultiMember(members)   => joinWith(",")(members.map(apply)
      case ExpressionMember(expr) => (new ExprTranslator)(expr)
      case StringMember(str)      => str
   }
}
