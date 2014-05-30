package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.Visitor


trait Translator[A <: ASTNode] extends Visitor[A, String] {
   implicit def setToList(set:Set[String]):Seq[String] = set.toList

   protected def joinWith(delim: String)(ss: Seq[String]): String =
      (new StringBuilder(ss.head) /: ss.tail)((buf, str) =>
         buf.append(delim).append(str)
      ).toString()

   protected def translateOp(op:String):String = op match {
      case "!=" => "\\neq"
      case "<=" => "\\le"
      case ">=" => "\\ge"
      case _ => op
   }

   protected def bracketedConditional(cond: String, t: String, f: String) =
      new StringBuilder()
            .append("\n\\begin{cases}\n")
            .append(s"$t, & \\ if \\ $cond \\\\ \n")
            .append(s"$f, & \\ otherwise \n")
            .append("\\end{cases} \n")
            .toString()
}
