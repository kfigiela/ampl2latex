package pl.edu.agh.mplt

import java.io.{PrintWriter, File}
import pl.edu.agh.mplt.parser.declaration.Declaration
import scala.annotation.tailrec
import pl.edu.agh.mplt.visitors.LatexTranslator

object App {
  def main(args: Array[String]) {
    if (args.size < 2) {
      println("Please specify input and output files")
    } else {
      val start = System.currentTimeMillis()
      val parsedFile = ParsedFile.fromAMPL(new File(args(0)))

      val out = new PrintWriter(new File(args(1)))

      println("starts")

      val tr = new LatexTranslator

      try {
        @tailrec
        def persist(str: Stream[Declaration]): Unit =
          if (!str.isEmpty) {
            out.println(tr.visit(str.head) + "\\\\")
            persist(str.tail)
          }
        println("prints")
        persist(parsedFile.ast)

      } catch {
        case e: Throwable =>
          out.write("\n error: " + e.getMessage)
          throw e
      } finally {
        out.close()
      }

      println("\n\n in: " + (System.currentTimeMillis() - start))
    }
  }
}
