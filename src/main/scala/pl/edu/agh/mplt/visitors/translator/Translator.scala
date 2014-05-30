package pl.edu.agh.mplt.visitors.translator

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.Visitor


trait Translator[A <: ASTNode] extends Visitor[A, String] {
   implicit def setToList(set: Set[String]): List[String] = set.toList

   protected def joinWith(delim: String)(ss: List[String]): String = ss match {
      case Nil      => ""
      case hd :: tl => (new StringBuilder(hd) /: tl)((buf, str) =>
         buf.append(delim).append(str)
      ).toString()
   }


   protected def translateOp(op: String): String = op match {
      case "!=" => "\\neq"
      case "<=" => "\\le"
      case ">=" => "\\ge"
      case _    => op
   }

   protected def bracketedConditional(cond: String, t: String, f: String) = new StringBuilder()
         .append("\n\\begin{cases}\n")
         .append(s"$t, & \\ if \\ $cond \\\\ \n")
         .append(s"$f, & \\ otherwise \n")
         .append("\\end{cases}")
         .toString()
}
