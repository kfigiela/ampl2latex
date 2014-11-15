package pl.edu.agh.mplt

import java.io.{PrintWriter, File}
import scala.annotation.tailrec

object App {
  def main(args: Array[String]) {
    if (args.size < 2) {
      println("Please specify input and output files")
      System.exit(1)
    }

    val parsedFile = Translator.fromAMPL(new File(args(0)))
    val out = new PrintWriter(new File(args(1)))

    try {
      parsedFile.translate.foreach(out.print)

      if (parsedFile.ast.errors.nonEmpty) {
        parsedFile.ast.printErrors()
        out.close()
        System.exit(1)
      }
    } catch {
      case e: Throwable =>
        out.write("\n error: " + e.getMessage)
        out.close()
        System.exit(1)
    } finally out.close()

    System.exit(0)
  }
}
