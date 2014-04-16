package pl.edu.agh.mplt

import java.io.{PrintWriter, File}

object App {
  def main(args: Array[String]) {
    if (args.size < 2) {
      println("Please specify input and output files")
    } else {
      val start = System.currentTimeMillis()
      val parsedFile = ParsedFile.fromAMPL(new File(args(0)))
      println("\n\n in: " + (System.currentTimeMillis() - start))

      val out = new PrintWriter(new File(args(1)))
      try {
        parsedFile.ast.foreach(line => out.println(line.toString))
        println(parsedFile.ast)

      } finally {out.close() }
    }
  }
}
