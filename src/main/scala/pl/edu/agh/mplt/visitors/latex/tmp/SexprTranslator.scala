package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.formula.set._
import pl.edu.agh.mplt.parser.formula.set.Sets.Intersection
import pl.edu.agh.mplt.parser.formula.set.Sets.SymetricDifference
import pl.edu.agh.mplt.parser.formula.set.Sets.Cartesian
import pl.edu.agh.mplt.parser.formula.set.SetExpressionIf
import pl.edu.agh.mplt.parser.formula.set.Sets.SetOf
import pl.edu.agh.mplt.parser.formula.set.Sets.Difference
import pl.edu.agh.mplt.parser.formula.set.Indexing
import pl.edu.agh.mplt.parser.formula.set.Sets.Union
import pl.edu.agh.mplt.parser.formula.set.IndexedSet
import pl.edu.agh.mplt.parser.formula.expression.Number
import pl.edu.agh.mplt.parser.reference.Reference
import pl.edu.agh.mplt.visitors.latex.ReferenceTranslator


class SexprTranslator extends Translator[SetExpression] {
   override def apply(node: SetExpression): String = node match {
      case ParenthesizedSetExpression(sexpr) => s"(${apply(sexpr) }})"

      case SetOf(indexing, member)         =>
         s"{ ${(new MemberTranslator)(member) } | ${(new IndexingTranslator)(indexing) }"
      case Union(left, right)              => s"{${apply(left) } \\bigcup ${apply(right) }"
      case Intersection(left, right)       => s"{${apply(left) } \\bigcap ${apply(right) }"
      case Difference(left, right)         => s"{${apply(left) } \\setminus ${apply(right) }"
      case SymetricDifference(left, right) => s"{${apply(left) } \\oplus ${apply(right) }"
      case Cartesian(left, right)          => s"{${apply(left) } \\times ${apply(right) }"

      case i@SetExpressionIf(_, _, _) => translatorIf(i)

      case IndexedSet(indexes, sexpr) =>
         s"{${(indexes.head /: indexes.tail)(_ + ", " + _) } \\in ${apply(sexpr) }}"
      case l@Indexing(_, _)           => (new IndexingTranslator)(l)

      case ExplicitSet(members) =>
         if (members.isEmpty) "\\{\\}"
         else new StringBuilder("\\{").append(joinWith(",")(members.map((new MemberTranslator)(_)))).append("\\}")
               .toString()

      case SetComprehension(start, end, Number("1")) =>
         s"{ x | x \\in [${(new MemberTranslator)(start) }, ${(new MemberTranslator)(end) }] }"

      case SetComprehension(start, end, step) =>
         s"[${(new MemberTranslator)(start) } to ${(new MemberTranslator)(end) }} by {${(new ExprTranslator)(step) }}]"

      case ref: Reference => (new ReferenceTranslator)(ref)

      case set => throw new Error(s"Unsupported node: $set")
   }

   private def translatorIf(sexpr: SetExpressionIf): String = {
      def parenthesize(sexpr: SetExpression) = sexpr match {
         case e@SetExpressionIf(_, _, _) => s"(${(new ExprTranslator)(e) }})"
         case e                          => (new ExprTranslator)(e)
      }

      val cond = (new LexprTranslator)(sexpr.lexpr)

      val t = apply(sexpr.left)
      val f = apply(sexpr.right)
      bracketedConditional(cond, t, f)
   }

   private def bracketedConditional(cond: String, t: String, f: String) =
      s"""
      |\begin{cases}
      |    \$t,& \text{if } $cond
      |    $f,              & \text{otherwise}
      |\end{cases}
    """.stripMargin
}
