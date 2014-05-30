package pl.edu.agh.mplt.visitors.latex.tmp

import pl.edu.agh.mplt.parser.ASTNode
import pl.edu.agh.mplt.visitors.latex.TmpVisitor


trait Translator[A <: ASTNode] extends TmpVisitor[A, String] {
   implicit def setToList(set:Set[String]):Seq[String] = set.toList

   protected def joinWith(delim: String)(ss: Seq[String]): String =
      (new StringBuilder(ss.head) /: ss.tail)((buf, str) =>
         buf.append(",").append(str)
      ).toString()

   protected def translateOp(op:String):String = op match {
      case "!=" => "\\neq"
      case "<=" => "\\le"
      case ">=" => "\\ge"
      case _ => op
   }
}
