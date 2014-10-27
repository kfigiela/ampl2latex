package pl.edu.agh.mplt

import java.io.{PrintWriter, File}
import scala.annotation.tailrec

object App {
  def main(args: Array[String]) {
    if (args.size < 2) {
      println("Please specify input and output files")
      System.exit(1)
    }
    val parsedFile = ParsedFile.fromAMPL(new File(args(0)))
    val out = new PrintWriter(new File(args(1)))

    try {
      parsedFile.translateVerbose.foreach(out.print)

      parsedFile.ast.printErrors()
    } catch {
      case e: Throwable =>
        out.write("\n error: " + e.getMessage)
        throw e
    } finally {
      out.close()
    }

  }
}
